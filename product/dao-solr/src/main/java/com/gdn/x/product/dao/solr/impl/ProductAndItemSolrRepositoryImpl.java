package com.gdn.x.product.dao.solr.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.service.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrQuery.SortClause;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.Group;
import org.apache.solr.client.solrj.response.GroupCommand;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.GroupParams;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.SolrGroupResultVO;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;

import static com.gdn.x.product.enums.SolrConstants.ROUTE_KEY;

@Component
@Slf4j
public class ProductAndItemSolrRepositoryImpl implements ProductAndItemSolrRepository {

  @Autowired
  @Qualifier("xproductClient")
  private CloudSolrClient cloudSolrClient;

  @Autowired
  @Qualifier("xproductL3Client")
  private CloudSolrClient cloudSolrClientL3;

  @Autowired
  private SystemParameterService systemParameterService;

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Value("${channel.default.value}")
  private String channelDefaultValue;

  @Value("${solr.max.row.size}")
  private int solrMaxRowSize;

  @Value("${solr.group.limit.productsku}")
  private int solrGroupLimitForProductSku;

  @Value("${event.based.solr.update.enabled}")
  private boolean eventBasedSolrUpdateEnable;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  private static final String ALL = "All";
  private static final String ACTIVE = "Active";
  private static final String SUSPENDED = "Suspended";
  private static final String COLLAPSE_QUERY_FOR_PRODUCT_SKU = "{!collapse field=productSku}";

  @Override
  public Page<ProductAndItemSolr> getListOfActiveProductSkusSortedByProductCode(String storeId, Pageable pageable) {
    SolrQuery solrQuery = constructSolrQueryToGetListOfActiveProductSkusSortedByProductCode(storeId, pageable);
    QueryResponse queryResponse;
    try {
      queryResponse = cloudSolrClient.query(solrQuery);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr for getting active products sorted by productCode ", e);
      throw new SolrCustomException(e.getMessage(), e);
    }
    List<ProductAndItemSolr> productAndItemSolrs;
    if (Objects.nonNull(queryResponse.getResults())) {
      productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return new PageImpl<>(productAndItemSolrs, pageable, queryResponse.getResults().getNumFound());
    }
    return new PageImpl<>(new ArrayList<>());
  }

  private SolrQuery constructSolrQueryToGetListOfActiveProductSkusSortedByProductCode(String storeId,
      Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_CODE, SolrFieldNames.ITEM_SKU,
        SolrFieldNames.ITEM_CODE, SolrFieldNames.IS_SYNCHRONIZED);
    solrQuery.setSort(SolrFieldNames.PRODUCT_CODE, SolrQuery.ORDER.asc);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setQuery(getStoreIdQuery(storeId));
    solrQuery.set(SolrConstants.GROUP, Boolean.TRUE);
    solrQuery.set(GroupParams.GROUP_FIELD, SolrFieldNames.PRODUCT_SKU);
    solrQuery.set(GroupParams.GROUP_MAIN, Boolean.TRUE);
    return solrQuery;
  }

  @Override
  public Page<ProductAndItemSolr> getProductAndItemsByFilter(String storeId, ItemSummaryRequestVO itemFilter,
      String orderBy, String sortBy, PageRequest pageRequest) {

    if (StringUtils.isNotBlank(itemFilter.getMasterCategoryCode())) {
      if (Objects.isNull(itemFilter.getCategoryCodes())) {
        itemFilter.setCategoryCodes(new ArrayList<>());
      }
      itemFilter.getCategoryCodes().add(itemFilter.getMasterCategoryCode());
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery = constructSolrQuery(storeId, itemFilter, solrQuery);
    if (StringUtils.isNotBlank(orderBy)) {
      solrQuery.setSorts(Arrays.asList(new SortClause(orderBy,
              SolrConstants.ASC.equalsIgnoreCase(sortBy) ? SolrQuery.ORDER.asc : SolrQuery.ORDER.desc),
          new SortClause(SolrFieldNames.ID, SolrQuery.ORDER.asc)));
    } else {
      solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    }
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.set(ROUTE_KEY, itemFilter.getMerchantCode());
    log.info("Solr query for active product listing : {}", solrQuery);
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      long totalNumFound = queryResponse.getResults().getNumFound();
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return new PageImpl<>(productAndItemSolrs, pageRequest, totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, ItemSummaryRequestVO {}", itemFilter, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductAndItemSolr> getPromoProductAndItemsByFilter(String storeId,
      ItemSummaryRequestVO itemSummaryRequestVO, String orderBy, String sortBy, PageRequest pageRequest) {
    SolrQuery solrQuery = new SolrQuery();
    constructSolrQueryForPromoItems(storeId, itemSummaryRequestVO, solrQuery);
    if (StringUtils.isNotBlank(orderBy)) {
      solrQuery.addSort(orderBy, SolrConstants.ASC.equals(sortBy) ? SolrQuery.ORDER.asc : SolrQuery.ORDER.desc);
    } else {
      solrQuery.addSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    }
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.set(ROUTE_KEY, itemSummaryRequestVO.getMerchantCode());
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      long totalNumFound = queryResponse.getResults().getNumFound();
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return new PageImpl<>(productAndItemSolrs, pageRequest, totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, ItemSummaryRequestVO {}", itemSummaryRequestVO, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private void constructSolrQueryForPromoItems(String storeId, ItemSummaryRequestVO itemSummaryRequestVO,
      SolrQuery solrQuery) {
    constructSolrQuery(storeId, itemSummaryRequestVO, solrQuery);
    if (CollectionUtils.isNotEmpty(itemSummaryRequestVO.getBoostProductSkus())) {
      StringBuilder boostProductSkus = new StringBuilder("productSku:");
      itemSummaryRequestVO.getBoostProductSkus()
          .forEach(boostProductSku -> boostProductSkus.append(StringUtils.SPACE).append(boostProductSku));
      solrQuery.set(SolrConstants.BQ, boostProductSkus.toString());
      solrQuery.set(SolrConstants.DEF_TYPE, SolrConstants.EDISMAX);
      solrQuery.addSort(SolrConstants.SORT_BOOST_QUERY, SolrQuery.ORDER.desc);
    }
  }

  @Override
  public Page<ProductAndItemSolr> getBulkProductAndItemsByFilter(String storeId, BulkItemSummaryRequestVo itemFilter,
      PageRequest pageRequest, String sortBy, String orderBy) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery = constructSolrQueryForBulkItemCodes(storeId, itemFilter, solrQuery);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.set(ROUTE_KEY, itemFilter.getMerchantCode());
    if (StringUtils.isNotBlank(orderBy)) {
      solrQuery.setSort(orderBy, SolrConstants.ASC.equals(sortBy) ? SolrQuery.ORDER.asc : SolrQuery.ORDER.desc);
    } else {
      solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    }
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      long totalNumFound = queryResponse.getResults().getNumFound();
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return new PageImpl<>(productAndItemSolrs, pageRequest, totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, BulkItemSummaryRequestVo {}", itemFilter, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public SolrGroupResultVO<ProductAndItemSolr> getProductsForOfficialStore(String storeId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    constructSolrQueryForOfficialStore(officialStoreRequestVO, solrQuery, storeId);
    Map<String, Integer> productSkuCountsMap = new HashMap<>();
    solrQuery = this.generateGroupQueryOfficialStore(pageable, solrQuery);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    try {
      QueryResponse queryResponse = cloudSolrClient.query(solrQuery);
      long totalDocuments = 0;
      if (Objects.nonNull(queryResponse.getGroupResponse()) && CollectionUtils
          .isNotEmpty(queryResponse.getGroupResponse().getValues())) {
        List<GroupCommand> groupCommands = queryResponse.getGroupResponse().getValues();
        for (GroupCommand groupCommand : groupCommands) {
          productSkuCountsMap.put(groupCommand.getName(), groupCommand.getNGroups());
          groupCommand.getValues().stream().map(Group::getResult).flatMap(SolrDocumentList::stream).
              map(CommonUtil::toProductAndItemSolr).collect(Collectors.toCollection(() -> productAndItemSolrs));
        }
      }
      if (productSkuCountsMap.containsKey(SolrFieldNames.PRODUCT_SKU)) {
        totalDocuments = productSkuCountsMap.get(SolrFieldNames.PRODUCT_SKU);
      }
      return new SolrGroupResultVO<ProductAndItemSolr>(productAndItemSolrs, pageable, totalDocuments);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, {}", officialStoreRequestVO);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public SolrGroupResultVO<ProductSolr> getProductsByCategoryAndMerchantCodeL3(String storeId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    constructSolrQueryForOfficialStore(officialStoreRequestVO, solrQuery, storeId);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setSort(SolrFieldNames.PRODUCT_SKU, SolrQuery.ORDER.asc);
    try {
      QueryResponse queryResponse = cloudSolrClientL3.query(solrQuery);
      return new SolrGroupResultVO<>(
          queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList()), pageable,
          queryResponse.getResults().getNumFound());
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, {}", officialStoreRequestVO);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public SolrGroupResultVO<ProductSolr> getActiveProductsListForMerchant(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    constructSolrQueryForMerchant(activeProductsRequestVO, solrQuery, storeId);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setSort(SolrFieldNames.CREATED_DATE, SolrQuery.ORDER.desc);
    if (StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode())) {
      solrQuery.set(ROUTE_KEY, activeProductsRequestVO.getMerchantCode());
    }
    List<ProductSolr> productSolrs = new ArrayList<>();
    long totalDocuments = 0;
    try {
      QueryResponse queryResponse = cloudSolrClientL3.query(solrQuery);
      if (CollectionUtils.isNotEmpty(queryResponse.getResults())) {
        totalDocuments = queryResponse.getResults().getNumFound();
        productSolrs = queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
      }
      return new SolrGroupResultVO<>(productSolrs, pageable, totalDocuments);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, {}", activeProductsRequestVO);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private void constructSolrQueryForOfficialStore(OfficialStoreRequestVO officialStoreRequestVO, SolrQuery solrQuery, String storeId) {
    StringBuilder query = new StringBuilder(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + getFQForListOfStrings(
        officialStoreRequestVO.getMerchantCodes()));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    if (StringUtils.isNotBlank(officialStoreRequestVO.getProductSku())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON)
          .append(appendDoubleQuotes(officialStoreRequestVO.getProductSku()));
    }
    String channel = this.channelDefaultValue;
    if (StringUtils.isNotBlank(officialStoreRequestVO.getChannelName())) {
      channel = officialStoreRequestVO.getChannelName();
    }

    if (Objects.isNull(officialStoreRequestVO.getOff2OnChannelActive()) || !officialStoreRequestVO.getOff2OnChannelActive()) {
      if(Objects.nonNull(officialStoreRequestVO.getOff2OnChannelActive())) {
        solrQuery.addFilterQuery(
            SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + officialStoreRequestVO.getOff2OnChannelActive());
      }
      if (Objects.nonNull(officialStoreRequestVO.getBuyable())) {
        solrQuery.addFilterQuery(SolrFieldNames.BUYABLE + SolrConstants.COLON + String
            .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
                officialStoreRequestVO.getBuyable()));
      }

      if (Objects.nonNull(officialStoreRequestVO.getDiscoverable())) {
        solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
            .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
                officialStoreRequestVO.getDiscoverable()));
      }
    } else {
      solrQuery.addFilterQuery(
          SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + officialStoreRequestVO.getOff2OnChannelActive());
    }

    if (StringUtils.isNotBlank(officialStoreRequestVO.getProductName())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PRODUCT_NAME)
          .append(SolrConstants.COLON)
          .append(ClientUtils.escapeQueryChars(officialStoreRequestVO.getProductName().trim()));
      solrQuery.set(SolrConstants.MM, 100);
    }
    if (CollectionUtils.isNotEmpty(officialStoreRequestVO.getBrands())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.BRAND + SolrConstants.COLON + getFQForListOfStrings(officialStoreRequestVO.getBrands()));
    }

    if (Objects.nonNull(officialStoreRequestVO.getArchived())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + officialStoreRequestVO.getArchived());
    }

    if (StringUtils.isNotBlank(officialStoreRequestVO.getCategoryCode())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(officialStoreRequestVO.getCategoryCode());
    }
    solrQuery.setQuery(query.toString());
  }

  private void constructSolrQueryForMerchant(ActiveProductsRequestVO activeProductsRequestVO, SolrQuery solrQuery,
      String storeId) {
    StringBuilder query = new StringBuilder();

    query.append(getStoreIdQuery(storeId));

    if (StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode()) && !Boolean.TRUE.equals(
        activeProductsRequestVO.getTradingProduct())) {
      query.append(SolrConstants.AND_CLAUSE + SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON)
          .append(activeProductsRequestVO.getMerchantCode());
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getCategoryCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, activeProductsRequestVO.getCategoryCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getPickupPointCodes())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PICKUP_POINT_CODES).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, activeProductsRequestVO.getPickupPointCodes()))
          .append(SolrConstants.CLOSING_BRACKET);
    }

    String channel = this.channelDefaultValue;
    if (Objects.nonNull(activeProductsRequestVO.getBuyable())) {
      solrQuery.addFilterQuery(SolrFieldNames.BUYABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
              activeProductsRequestVO.getBuyable()));
    }

    if (Objects.nonNull(activeProductsRequestVO.getTradingProduct())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.TRADING_PRODUCT + SolrConstants.COLON + activeProductsRequestVO.getTradingProduct());
    }

    if (Objects.nonNull(activeProductsRequestVO.getBundleProduct())) {
      if (Boolean.TRUE.equals(activeProductsRequestVO.getBundleProduct())) {
        solrQuery.addFilterQuery(
            SolrFieldNames.BUNDLE_PRODUCT + SolrConstants.COLON + activeProductsRequestVO.getBundleProduct());
      } else {
        solrQuery.addFilterQuery(
            SolrConstants.NOT_IN + SolrFieldNames.BUNDLE_PRODUCT + SolrConstants.COLON + Boolean.TRUE);
      }
    }

    if (Objects.nonNull(activeProductsRequestVO.getDiscoverable())) {
      solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
              activeProductsRequestVO.getDiscoverable()));
    }

    if(Objects.nonNull(activeProductsRequestVO.getCncActivated())) {
      solrQuery.addFilterQuery(SolrFieldNames.CNC_ACTIVE + SolrConstants.COLON +
              activeProductsRequestVO.getCncActivated());
    }

    if (StringUtils.isNotBlank(activeProductsRequestVO.getSearchKey())) {
      query.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(activeProductsRequestVO.getSearchKey().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.PRODUCT_NAME + StringUtils.SPACE + SolrFieldNames.PRODUCT_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }

    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setQuery(query.toString());
  }

  @Deprecated
  @Override
  public Page<ProductAndItemSolr> getListItemsByProductSkus(String storeId, List<String> productSkus,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) {
    SolrQuery solrQuery = constructSolrQueryToGetListOfItemsByProductSkus(storeId, productSkus, officialStoreRequestVO, pageable);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      long totalNumFound = queryResponse.getResults().getNumFound();
      return new PageImpl<>(productAndItemSolrs, pageable, totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data from solr, productSkus {}", productSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private SolrQuery constructSolrQueryToGetListOfItemsByProductSkus(String storeId, List<String> productSkus,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(getStoreIdQuery(storeId));
    if (CollectionUtils.isNotEmpty(productSkus)) {
      String subQuery = SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(productSkus);
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, false);
    }
    String channel = this.channelDefaultValue;
    if (StringUtils.isNotBlank(officialStoreRequestVO.getChannelName())) {
      channel = officialStoreRequestVO.getChannelName();
    }
    if (Objects.nonNull(officialStoreRequestVO.getArchived())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + officialStoreRequestVO.getArchived());
    }

    if (Objects.nonNull(officialStoreRequestVO.getBuyable())) {
      String subQuery = SolrFieldNames.BUYABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
              officialStoreRequestVO.getBuyable());
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, false);
    }

    if (Objects.nonNull(officialStoreRequestVO.getDiscoverable())) {
      String subQuery = SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter,
              officialStoreRequestVO.getDiscoverable());
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, false);
    }
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.OFFER_PRICE, SolrFieldNames.LIST_PRICE,
        SolrFieldNames.ITEM_SKU);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    checkStoreIdQueryAndSetFilterQuery(storeId, solrQuery);
    return solrQuery;
  }

  @Override
  @Deprecated
  public Page<ProductAndItemSolr> getProductsByMasterCatalog(String storeId, String catalogCode, String categoryCode,
      boolean searchEmptySalesOnly, Pageable page) {
    SolrQuery solrQuery =
        constructSolrQueryToGetProductsByMasterCatalog(storeId, catalogCode, categoryCode, searchEmptySalesOnly, page);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      long totalNumFound = queryResponse.getResults().getNumFound();
      return new PageImpl<>(productAndItemSolrs, page, totalNumFound);

    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting Products By MasterCatalog, {}, {}", catalogCode, categoryCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private SolrQuery constructSolrQueryToGetProductsByMasterCatalog(String storeId, String catalogCode,
      String categoryCode, boolean searchEmptySalesOnly, Pageable page) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.QUOTES + String
        .format(SolrConstants.STRING_FORMATTER, catalogCode, this.solrStringDelimiter, categoryCode)
        + SolrConstants.QUOTES);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    if (searchEmptySalesOnly) {
      solrQuery.setQuery(solrQuery.getQuery() + SolrConstants.AND_CLAUSE + SolrConstants.NULL_SALES_CATALOG);
    }
    solrQuery = this.addGroupQuery(page, solrQuery);
    return solrQuery;
  }

  @Override
  public Page<ProductAndItemSolr> getProductsBySalesCatalog(String storeId, String catalogCode, String categoryCode,
      Pageable page) throws Exception{
    SolrQuery solrQuery = constructSolrQueryToGetProductsBySalesCatalog(storeId, catalogCode, categoryCode);
    solrQuery.setRows(page.getPageSize());
    solrQuery.setStart(page.getPageNumber() * page.getPageSize());
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      long totalNumFound = queryResponse.getResults().getNumFound();
      return new PageImpl<>(productAndItemSolrs, page, totalNumFound);

    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting products By Sales Catalog :{}, {} ", catalogCode, categoryCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private SolrQuery constructSolrQueryToGetProductsBySalesCatalog(String storeId, String catalogCode,
      String categoryCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.SALES_CATALOG + SolrConstants.COLON + SolrConstants.QUOTES + String
        .format(SolrConstants.STRING_FORMATTER, catalogCode, this.solrStringDelimiter, categoryCode)
        + SolrConstants.QUOTES);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.CREATED_DATE, SolrQuery.ORDER.asc);
    return solrQuery;
  }

  @Override
  public Page<ProductAndItemSolr> getProductsWithNullSalesCatalogAndMarkForDeleteFalse(String storeId, DateTime startDate,
      DateTime endDate, Pageable page) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery =
        constructSolrQuerygetProductsWithNullSalesCatalogAndMarkForDeleteFalse(storeId, startDate, endDate, page,
            solrQuery);
    try {
      QueryResponse queryResponse = this.cloudSolrClientL3.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      long totalNumFound = queryResponse.getResults().getNumFound();
      return new PageImpl<>(productAndItemSolrs, page, totalNumFound);
    } catch (SolrServerException | SolrException | IOException e) {
      log.error(
          "Exception caught while getting Products With Null Sales Catalog And Mark For Delete False startDate:{} , "
              + "endDate:{}", startDate, endDate, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private SolrQuery constructSolrQuerygetProductsWithNullSalesCatalogAndMarkForDeleteFalse(String storeId,
      DateTime startDate, DateTime endDate, Pageable page, SolrQuery solrQuery) {
    String createdDateQuery = new StringBuilder().append(SolrFieldNames.CREATED_DATE).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_SQUARE_BRACES).append(startDate)
        .append(SolrConstants.TO_CLAUSE).append(endDate).append(StringUtils.SPACE)
        .append(SolrConstants.CLOSE_SQUARE_BRACES).toString();
    solrQuery.setQuery(createdDateQuery);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setQuery(solrQuery.getQuery() + SolrConstants.AND_CLAUSE + SolrConstants.NULL_SALES_CATALOG);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.CREATED_DATE, SolrQuery.ORDER.asc);
    solrQuery = addFieldsForGetProductsWithNullSalesCatalog(page, solrQuery);
    return solrQuery;
  }

  @Override
  public Page<ProductAndItemSolr> getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(
      String storeId, String merchantCode, List<String> categories, List<String> brands, String keyword, String itemSku,
      PageRequest pageRequest) {
    String channel = this.channelDefaultValue;
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.set(ROUTE_KEY, merchantCode);
    boolean containQuery =
        isContainQuery(storeId, merchantCode, categories, brands, keyword, itemSku, channel, solrQuery);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    if (containQuery) {
      try {
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery, SolrRequest.METHOD.POST);
        List<ProductAndItemSolr> productAndItemSolrs =
            queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
        long totalNumFound = queryResponse.getResults().getNumFound();
        return new PageImpl<>(productAndItemSolrs, pageRequest, totalNumFound);

      } catch (SolrServerException | IOException | SolrException e) {
        log.error("Exception caught while getting Products With Merchant Code And MasterCatalog, merchantCode:{}",
            merchantCode, e);
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
    return new PageImpl<>(new ArrayList<>());
  }

  private boolean isContainQuery(String storeId, String merchantCode, List<String> categories, List<String> brands,
      String keyword, String itemSku, String channel, SolrQuery solrQuery) {
    boolean containQuery = false;
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.BUYABLE + SolrConstants.COLON + String
        .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, Boolean.TRUE));
    solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
        .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, Boolean.TRUE));
    solrQuery.setQuery(getStoreIdQuery(storeId));
    if (StringUtils.isNotBlank(itemSku)) {
      String subQuery = SolrFieldNames.ITEM_SKU + SolrConstants.COLON + itemSku;
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
      containQuery = true;
    }
    if (StringUtils.isNotBlank(merchantCode)) {
      String subQuery = SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + merchantCode;
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
      containQuery = true;
    }
    if (StringUtils.isNotBlank(keyword)) {
      solrQuery.setQuery(new StringBuilder(solrQuery.getQuery()).append(SolrConstants.AND_CLAUSE)
          .append(SolrFieldNames.ITEM_NAME + SolrConstants.COLON + ClientUtils.escapeQueryChars(keyword.trim())).toString());
      containQuery = true;
      solrQuery.set(SolrConstants.MM, 100);
    }
    if (CollectionUtils.isNotEmpty(categories)) {
      solrQuery.setQuery(
          new StringBuilder(solrQuery.getQuery()).append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG)
              .append(SolrConstants.COLON).append(getFQForListOfStrings(categories)).toString());
      containQuery = true;
    }
    if (CollectionUtils.isNotEmpty(brands)) {
      solrQuery.addFilterQuery(SolrFieldNames.BRAND + SolrConstants.COLON + getFQForListOfStrings(brands));
      containQuery = true;
    }
    return containQuery;
  }

  @Override
  @Deprecated
  public void deleteOfflineItemByItemIds(List<String> itemIds) {
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    for (String id : itemIds) {
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, Collections.singletonMap(SolrConstants.SET_CLAUSE, id));
      solrInputDocument.setField(SolrFieldNames.OFFLINE_PRICES,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, new ArrayList<>()));
      solrInputDocument
          .setField(SolrFieldNames.CNC_ACTIVE, Collections.singletonMap(SolrConstants.SET_CLAUSE, Boolean.FALSE));
      solrInputDocuments.add(solrInputDocument);
    }
    try {
      this.cloudSolrClient.add(solrInputDocuments);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while updating documents to solr :{}", solrInputDocuments, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductAndItemSolr> getItemsByMerchantCode(String storeId, String merchantCode,
      Pageable pageRequest) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + merchantCode);
    solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String.format(
        SolrConstants.STRING_FORMATTER, this.channelDefaultValue, this.solrStringDelimiter, true));
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + false);
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.setFields(SolrFieldNames.ID);
    solrQuery.set(ROUTE_KEY, merchantCode);
    log.debug("Solr Query for getting item sku : {}", solrQuery);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      log.debug("query response mv {}", queryResponse);
      if(Objects.nonNull(queryResponse)) {
        SolrDocumentList solrDocuments = queryResponse.getResults();
        return new PageImpl<>(solrDocuments.stream()
            .map(CommonUtil::toProductAndItemSolr)
            .collect(Collectors.toList()), pageRequest, solrDocuments.getNumFound());
      } else {
        return new PageImpl<>(Collections.emptyList());
      }

    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting itemSkus by merchantCode:{}", merchantCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public void executeSolrDocumentAtomicUpdate(SolrInputDocument solrInputDocument) throws Exception {
    try {
      cloudSolrClient.add(solrInputDocument);
    } catch (SolrServerException | IOException | CloudSolrClient.RouteException e) {
      log.error("Failure while performing atomic update on solrInputDocument: {}", solrInputDocument, e);
      throw e;
    }
  }

  @Override
  public void executeSolrDocumentsAtomicUpdate(List<SolrInputDocument> solrInputDocuments) throws Exception {
    try {
      cloudSolrClient.add(solrInputDocuments);
    } catch (SolrServerException | IOException | CloudSolrClient.RouteException e) {
      log.error("Failure while performing atomic updates on solrInputDocuments: {}", solrInputDocuments, e);
      throw e;
    }
  }

  @Override
  public Page<ProductAndItemSolr> getProductsByMerchantCodeAndCategoryCodesAndStatus(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest) {
    SolrQuery solrQuery = constructSolrQueryForProductSuspension(storeId, activeProductsRequestVO);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.MERCHANT_CODE, SolrFieldNames.IS_SUSPENDED,
        SolrFieldNames.MARK_FOR_DELETE);
    solrQuery.set(ROUTE_KEY, activeProductsRequestVO.getMerchantCode());
    log.debug("Solr Query for getting product sku for suspension : {}", solrQuery);
    try {
      QueryResponse queryResponse;
      if ((Boolean.valueOf(systemParameterService
          .findValueByStoreIdAndVariable(storeId, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION).getValue()))) {
        queryResponse = cloudSolrClientL3.query(solrQuery);
      } else {
        queryResponse = this.cloudSolrClient.query(solrQuery);
      }
      if (Objects.nonNull(queryResponse)) {
        SolrDocumentList solrDocuments = queryResponse.getResults();
        return new PageImpl<>(solrDocuments.stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()),
            pageRequest, solrDocuments.getNumFound());
      } else {
        return new PageImpl<>(Collections.emptyList());
      }
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting product Skus for request:{}", activeProductsRequestVO, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Page<ProductAndItemSolr> getProductsByMerchantCodeAndCategoryCodesAndStatusV2(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest) {
    SolrQuery solrQuery = getProductsByMerchantCodeAndCategoryCodesSolrQuery(storeId, activeProductsRequestVO);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.MERCHANT_CODE, SolrFieldNames.IS_SUSPENDED,
        SolrFieldNames.MARK_FOR_DELETE);

    log.debug("Solr Query for getting product sku for suspension : {}", solrQuery);
    try {
      QueryResponse queryResponse = cloudSolrClientL3.query(solrQuery);
      if (Objects.nonNull(queryResponse)) {
        SolrDocumentList solrDocuments = queryResponse.getResults();
        return new PageImpl<>(solrDocuments.stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()),
            pageRequest, solrDocuments.getNumFound());
      } else {
        return new PageImpl<>(Collections.emptyList());
      }
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting product Skus for request:{}", activeProductsRequestVO, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private SolrQuery getProductsByMerchantCodeAndCategoryCodesSolrQuery(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder(getStoreIdQuery(storeId));

    if (StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode())) {
      solrQuery.set(ROUTE_KEY, activeProductsRequestVO.getMerchantCode());
      solrQuery.addFilterQuery(
          SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + activeProductsRequestVO.getMerchantCode());
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getCategoryCodes())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + getFQForListOfStrings(
              activeProductsRequestVO.getCategoryCodes()) + SolrConstants.CLOSING_BRACKET);
    }

    if (ALL.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.MARK_FOR_DELETE).append(SolrConstants.COLON).append(Boolean.FALSE)
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.OR_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.IS_SUSPENDED).append(SolrConstants.COLON).append(Boolean.TRUE)
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET).toString());
    } else if (ACTIVE.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MARK_FOR_DELETE)
              .append(SolrConstants.COLON).append(Boolean.FALSE).append(SolrConstants.CLOSING_BRACKET).toString());
    } else if (SUSPENDED.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.IS_SUSPENDED)
              .append(SolrConstants.COLON).append(Boolean.TRUE).append(SolrConstants.CLOSING_BRACKET).toString());
    }

    if (StringUtils.isNotBlank(activeProductsRequestVO.getSearchKey())) {
      query.append(SolrConstants.AND_CLAUSE).append(
              SolrConstants.OPEN_BRACKET + SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE)
          .append(activeProductsRequestVO.getSearchKey().trim()).append(SolrConstants.DOUBLE_QUOTE)
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.PRODUCT_CODE).append(SolrConstants.COLON)
          .append(SolrConstants.DOUBLE_QUOTE).append(activeProductsRequestVO.getSearchKey().trim()).append(SolrConstants.DOUBLE_QUOTE)
          .append(SolrConstants.OR_CLAUSE).append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.DOUBLE_QUOTE).append(activeProductsRequestVO.getSearchKey().trim())
          .append(SolrConstants.DOUBLE_QUOTE).append(SolrConstants.CLOSING_BRACKET);
    }

    solrQuery.setQuery(query.toString());
    log.info("Solr for suspension for request: {} and is : {} ", activeProductsRequestVO, solrQuery.getQuery());
    return solrQuery;
  }

  @Override
  public Page<ProductAndItemSolr> getItemsByMerchantCodeAndCategoryCodes(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageRequest) {
    SolrQuery solrQuery = constructSolrQueryForSuspension(storeId, activeProductsRequestVO, false);
    solrQuery.setStart(pageRequest.getPageNumber() * pageRequest.getPageSize());
    solrQuery.setRows(pageRequest.getPageSize());
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.ITEM_SKU, SolrFieldNames.PRODUCT_CODE, SolrFieldNames.MERCHANT_CODE,
        SolrFieldNames.IS_SUSPENDED, SolrFieldNames.ITEM_NAME, SolrFieldNames.ITEM_IMAGES);
    solrQuery.set(ROUTE_KEY, activeProductsRequestVO.getMerchantCode());
    log.debug("Solr Query for getting item sku : {}", solrQuery);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      if (Objects.nonNull(queryResponse)) {
        SolrDocumentList solrDocuments = queryResponse.getResults();
        return new PageImpl<>(solrDocuments.stream()
            .map(CommonUtil::toProductAndItemSolr)
            .collect(Collectors.toList()), pageRequest, solrDocuments.getNumFound());
      } else {
        return new PageImpl<>(Collections.emptyList());
      }
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting itemSkus for request:{}", activeProductsRequestVO, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public Map<String, Long> getItemsByProductSkus(String storeId, List<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return Collections.emptyMap();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.set(SolrConstants.FACET, String.valueOf(Boolean.TRUE));
    solrQuery.set(SolrConstants.FACET_FIELD, SolrFieldNames.PRODUCT_SKU);
    solrQuery.setFacetLimit(productSkus.size());
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(productSkus));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setStart(0);
    solrQuery.setRows(0);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<FacetField> facetFields = queryResponse.getFacetFields();
      log.info("facet fields : {}", facetFields);
      Map<String, Long> productSkuMap = new HashMap<>();
      if (Objects.nonNull(queryResponse) && CollectionUtils.isNotEmpty(queryResponse.getFacetFields())
          && CollectionUtils.isNotEmpty(queryResponse.getFacetFields().get(0).getValues())) {
        for (FacetField.Count count : facetFields.get(0).getValues()) {
          productSkuMap.put(count.getName(), count.getCount());
        }
      }
      return productSkuMap;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productSku:{}", productSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findByProductCode(String productCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + productCode);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(
            queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting data by productCode:{}", productCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByProductSku(String productSku) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku);
    solrQuery.setStart(0);
    solrQuery.setRows(solrMaxRowSize);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      return queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while getting data by productSku: {} ", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Set<ProductAndItemSolr> findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(String storeId,
      String merchantCode, boolean off2OnChannelActive) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + merchantCode);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + off2OnChannelActive);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.add(ROUTE_KEY, merchantCode);
    try {
      int start = 0;
      long totalNumFound;
      Set<ProductAndItemSolr> productAndItemSolrs = new HashSet<>();
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(
            queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toSet()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, merchantCode:{} and off2on:{}", merchantCode,
          off2OnChannelActive, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String pickupPointCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME);
    solrQuery.setQuery(SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON + pickupPointCode);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, pickupPointCode:{} ", pickupPointCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public Long getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String pickupPointCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON + pickupPointCode);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setStart(0);
    solrQuery.setRows(0);
    try {
      long totalNumFound = 0;
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      if(Objects.nonNull(queryResponse.getResults())) {
        totalNumFound = queryResponse.getResults().getNumFound();
      }
      return totalNumFound;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, pickupPointCode:{} ", pickupPointCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(String storeId,
      Set<String> productCatentryIds) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(getStoreIdQuery(storeId));
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.IS_SYNCHRONIZED);
    if (CollectionUtils.isEmpty(productCatentryIds)) {
      return Collections.emptyList();
    }
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + getFQForListOfStrings(
        new ArrayList<>(productCatentryIds)));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, parentCatentryId:{}", productCatentryIds, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, String productCode, String merchantCode) {
    int start = 0;
    long totalNumFound;
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + productCode);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.IS_SYNCHRONIZED);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.addFilterQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + appendDoubleQuotes(merchantCode));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    try {
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(
            queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productCode:{}, merchantCode:{}", productCode,
          merchantCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId,
      Set<String> productCodes) {
    int start = 0;
    long totalNumFound;
    if (CollectionUtils.isEmpty(productCodes)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + getFQForListOfStrings(new ArrayList<>(productCodes)));
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.IS_SYNCHRONIZED, SolrFieldNames.PRODUCT_NAME);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
    try {
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(
            queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productCodes:{}", productCodes, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(
      String storeId, Set<String> productCodes) {
    if (CollectionUtils.isEmpty(productCodes)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(
          SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + getFQForListOfStrings(new ArrayList<>(productCodes)));
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productCodes:{}", productCodes, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findByStoreIdAndProductSku(String storeId, String productSku,
    String merchantCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.IS_SYNCHRONIZED, SolrFieldNames.PRODUCT_NAME,
        SolrFieldNames.PICKUP_POINT_CODE, SolrFieldNames.WHOLESALE_PRICE_ACTIVATED, SolrFieldNames.MERCHANT_CODE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.set(ROUTE_KEY, merchantCode);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productSku:{}", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
    String merchantCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.add(ROUTE_KEY, merchantCode);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productSku:{}", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
      List<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(productSkus));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productSku:{}", productSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public List<ProductAndItemSolr> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId,
      Set<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return Collections.emptyList();
    }
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(new ArrayList<>(productSkus)));
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE,
        SolrFieldNames.PRODUCT_CODE, SolrFieldNames.IS_SYNCHRONIZED);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productSku:{}", productSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findByTicketTemplateCodeAndMarkForDeleteFalse(String ticketTemplateCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.TICKET_TEMPLATE_CODE + SolrConstants.COLON + appendDoubleQuotes(ticketTemplateCode));
    solrQuery.setFields(SolrFieldNames.ITEM_SKU);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, ticketTemplateCode:{}", ticketTemplateCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public ProductAndItemSolr findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(String storeId,
      String productCatentryId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + productCatentryId);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return productAndItemSolrs.stream().findFirst().get();
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, productCatenryId:{}", productCatentryId, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findItemSkuAndCodeByStoreIdAndItemSkuIn(String storeId, Set<String> itemSkus) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(getStoreIdQuery(storeId));
    solrQuery.setFields(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      String subQuery = SolrFieldNames.ITEM_SKU + SolrConstants.COLON + getFQForListOfStrings(new ArrayList<>(itemSkus));
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, false);
    }
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, itemSkus:{}", itemSkus, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public Set<String> getItemSkusByPristineId(String storeId, String pristineId) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(
        SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + SolrConstants.QUOTES + pristineId + SolrConstants.QUOTES);
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setFields(SolrFieldNames.ITEM_SKU);
    try {
      Set<String> itemSkus = new HashSet<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        itemSkus.addAll(queryResponse.getResults().stream()
            .filter(solrDocument -> Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU)))
            .map(solrDocument -> String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU)))
            .collect(Collectors.toSet()));
        start++;
        totalNumFound = queryResponse.getResults().getNumFound();
      } while ((start * solrMaxRowSize) < totalNumFound);
      return itemSkus;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, pristineId:{}", pristineId, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Deprecated
  public ProductAndItemSolr findFirstByStoreIdAndItemSku(String storeId, String itemSku) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + itemSku);
    solrQuery.setFields(SolrFieldNames.ITEM_SKU, SolrFieldNames.ITEM_CODE);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      List<ProductAndItemSolr> productAndItemSolrs =
          queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList());
      return productAndItemSolrs.stream().findFirst().get();
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting first record by storeId, itemSku:{}", itemSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public List<ProductAndItemSolr> findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId,
      String merchantCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + merchantCode);
    solrQuery.setFields(SolrFieldNames.ID);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    solrQuery.set(ROUTE_KEY, merchantCode);
    try {
      List<ProductAndItemSolr> productAndItemSolrs = new ArrayList<>();
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        productAndItemSolrs.addAll(queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).collect(Collectors.toList()));
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
      return productAndItemSolrs;
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while getting data by storeId, merchantCode:{}", merchantCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public void save(ProductAndItemSolr productAndItemSolr) {
    SolrInputDocument solrInputFields = CommonUtil.toSolrInputDocument(productAndItemSolr);
    try {
      this.cloudSolrClient.add(solrInputFields);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while adding data to solr productCode:{}", productAndItemSolr.getProductCode(), e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Async
  public void updateCncActivatedByMerchantCodeSolr(String storeId, String merchantCode, boolean cncActivated) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + merchantCode);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.set(ROUTE_KEY, merchantCode);
    Date date = new Date();
    try {
      int start = 0;
      long totalNumFound;
      do {
        solrQuery.setStart(start * solrMaxRowSize);
        solrQuery.setRows(solrMaxRowSize);
        QueryResponse queryResponse = this.cloudSolrClientL3.query(solrQuery);
        totalNumFound = queryResponse.getResults().getNumFound();
        List<ProductSolr> productSolrs =
            queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
        if (eventBasedSolrUpdateEnable) {
          for (ProductSolr productSolr : productSolrs) {
            Map<String, Object> fieldsAndValues = new HashMap<>();
            fieldsAndValues.put(SolrFieldNames.CNC_ACTIVE, cncActivated);
            log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
                ProductDomainEventName.UPDATE_TO_SOLR, productSolr.getProductSku(), fieldsAndValues);
            kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSolr.getProductSku(),
              new ProductAndItemEventModel(productSolr.getProductSku(), fieldsAndValues,
                productSolr.getMerchantCode()));
          }
        } else {
          for (ProductSolr productSolr : productSolrs) {
            productSolr.setCncActive(cncActivated);
            productSolr.setUpdatedDate(date);
          }
          List<SolrInputDocument> solrInputDocumentList =
              productSolrs.stream().map(CommonUtil::toSolrInputDocument).collect(Collectors.toList());
          this.cloudSolrClientL3.add(solrInputDocumentList);
        }
        start++;
      } while ((start * solrMaxRowSize) < totalNumFound);
    } catch (SolrServerException | IOException | SolrException e) {
      log.error("Exception caught while updating data by storeId, merchantCode : {} , Error - ", merchantCode, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public void updateCncActivatedByProductSkuSetSolr(String storeId, Set<String> productSkuSet, boolean cncActivated,
    String merchantCode) {
    if (eventBasedSolrUpdateEnable) {
      for (String productSku : productSkuSet) {
        Map<String, Object> fieldsAndValues = new HashMap<>();
        fieldsAndValues.put(SolrFieldNames.CNC_ACTIVE, cncActivated);
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
            ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,productSku,
          new ProductAndItemEventModel(productSku, fieldsAndValues, merchantCode));
      }
    } else {
      SolrQuery solrQuery = new SolrQuery();
      List<String> productSkuList = productSkuSet.stream().collect(Collectors.toList());
      solrQuery.setQuery(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(productSkuList));
      solrQuery.addFilterQuery(getStoreIdQuery(storeId));
      Date date = new Date();
      try {
        int start = 0;
        long totalNumFound;
        do {
          solrQuery.setStart(start * solrMaxRowSize);
          solrQuery.setRows(solrMaxRowSize);
          QueryResponse queryResponse = this.cloudSolrClientL3.query(solrQuery);
          totalNumFound = queryResponse.getResults().getNumFound();
          List<ProductSolr> productSolrs =
              queryResponse.getResults().stream().map(CommonUtil::toProductSolr).collect(Collectors.toList());
          for (ProductSolr productSolr : productSolrs) {
            productSolr.setCncActive(cncActivated);
            productSolr.setUpdatedDate(date);
          }
          List<SolrInputDocument> solrInputDocumentList =
              productSolrs.stream().map(CommonUtil::toSolrInputDocument).collect(Collectors.toList());
          this.cloudSolrClientL3.add(solrInputDocumentList);
          start++;
        } while ((start * solrMaxRowSize) < totalNumFound);
      } catch (SolrServerException | IOException | SolrException e) {
        for (String productSku : productSkuList) {
          log.error("Exception caught while updating L3 solr data by storeId : {} , productSku : {} , Error - ", storeId,
              productSku, e);
        }
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
  }

  @Override
  public ProductAndItemSolr findOne(String itemSku, String merchantCode) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + itemSku);
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.set(ROUTE_KEY, merchantCode);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    try {
      QueryResponse queryResponse = this.cloudSolrClient.query(solrQuery);
      return queryResponse.getResults().stream().map(CommonUtil::toProductAndItemSolr).findFirst().orElse(null);
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by itemSku : {}", itemSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  private String appendDoubleQuotes(String string) {
    return new StringBuilder(SolrConstants.DOUBLE_QUOTE).append(string).append(SolrConstants.DOUBLE_QUOTE).toString();
  }

  private String getFQForListOfStrings(List<String> strings) {
    return new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrConstants.DOUBLE_QUOTE)
        .append(String.join(SolrConstants.COMMA_WITH_QUOTES, strings)).append(SolrConstants.DOUBLE_QUOTE)
        .append(SolrConstants.CLOSING_BRACKET).toString();
  }


  private String addPickupPointCodeInSolrQuery(String pickupPointCode) {
    String[] pickupPointCodes = pickupPointCode.split(SolrConstants.COMMA);
    return new StringBuilder().append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.OFFLINE_PRICES)
            .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, pickupPointCodes))
            .append(SolrConstants.CLOSING_BRACKET).toString();
  }

  private String addPickupPointCodesInSolrQuery(List<String> pickupPointCodes) {
    return new StringBuilder().append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.OFFLINE_PRICES)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
        .append(String.join(SolrConstants.OR_CLAUSE, pickupPointCodes))
        .append(SolrConstants.CLOSING_BRACKET).toString();
  }

  private SolrQuery constructSolrQuery(String storeId, ItemSummaryRequestVO itemFilter, SolrQuery solrQuery) {
    if (StringUtils.isNotBlank(itemFilter.getMerchantSku())) {
      if (CollectionUtils.isNotEmpty(itemFilter.getMerchantSkus())) {
        itemFilter.getMerchantSkus().add(itemFilter.getMerchantSku());
      } else {
        itemFilter.setMerchantSkus(Collections.singletonList(itemFilter.getMerchantSku()));
      }
    }
    solrQuery.setQuery(getStoreIdQuery(storeId));
    if (StringUtils.isNotBlank(itemFilter.getItemCode())) {
        String subQuery = SolrFieldNames.ITEM_CODE + SolrConstants.COLON + itemFilter.getItemCode();
        checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    if (StringUtils.isNotBlank(itemFilter.getProductCode())) {
      String subQuery = SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + appendDoubleQuotes(itemFilter.getProductCode());
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    if (StringUtils.isNotBlank(itemFilter.getMerchantCode())) {
      String subQuery = SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + itemFilter.getMerchantCode();
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    if (!Boolean.TRUE.equals(itemFilter.getMarkForDelete())) {
      solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    }
    if (Objects.nonNull(itemFilter.getArchived())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + itemFilter.getArchived());
    }

    if (Objects.nonNull(itemFilter.getFreeSample())){
      solrQuery.addFilterQuery(SolrFieldNames.FREE_SAMPLE + SolrConstants.COLON + itemFilter.getFreeSample());
    }

    String channel = this.channelDefaultValue;
    if (StringUtils.isNotBlank(itemFilter.getChannelName())) {
      channel = itemFilter.getChannelName();
    }
    if(Objects.isNull(itemFilter.getOff2OnChannelActive()) || !itemFilter.getOff2OnChannelActive()) {
      if(Objects.nonNull(itemFilter.getOff2OnChannelActive())) {
        solrQuery.addFilterQuery(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + itemFilter.getOff2OnChannelActive());
      }
      if (Objects.nonNull(itemFilter.getBuyable())) {
        solrQuery.addFilterQuery(SolrFieldNames.BUYABLE + SolrConstants.COLON + String
            .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, itemFilter.getBuyable()));
      }
      if (Objects.nonNull(itemFilter.getDiscoverable())) {
        solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
            .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, itemFilter.getDiscoverable()));
      }
    }
    else {
      solrQuery.addFilterQuery(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + itemFilter.getOff2OnChannelActive());
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getProductSkus())) {
      List<String> productSkus =
          itemFilter.getProductSkus().stream().filter(StringUtils::isNotBlank).collect(Collectors.toList());
      String subQuery = SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + getFQForListOfStrings(productSkus);
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getMerchantSkus())) {
      String subQuery =
          SolrFieldNames.MERCHANT_SKU + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getMerchantSkus());
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getItemSkus())) {
      String subQuery =
          SolrFieldNames.ITEM_SKU + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getItemSkus());
      checkStoreIdQueryAndSetQuery(storeId, solrQuery, subQuery, true);
    }
    StringBuilder defaultQuery = new StringBuilder(solrQuery.getQuery());
    if (CollectionUtils.isNotEmpty(itemFilter.getCategoryCodes())) {
      List<String> categoryCodes =
          itemFilter.getCategoryCodes().stream().filter(StringUtils::isNotBlank).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(categoryCodes)) {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET)
            .append(String.join(SolrConstants.OR_CLAUSE, categoryCodes))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getExcludedItemSkus())) {
      List<String> excludedItems = itemFilter.getExcludedItemSkus().stream()
        .filter(StringUtils::isNotBlank)
        .collect(Collectors.toList());

      if (!excludedItems.isEmpty()) {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.NOT_IN)
          .append(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + getFQForListOfStrings(excludedItems));
      }
    }

    if (StringUtils.isNotBlank(itemFilter.getLinkedPartnerCode())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.NOT_IN)
        .append(SolrFieldNames.LINKED_PARTNERS + SolrConstants.COLON + getFQForListOfStrings(
          Arrays.asList(itemFilter.getLinkedPartnerCode())));
    }

    if (StringUtils.isNotBlank(itemFilter.getItemSkuKeyword())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.ITEM_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(itemFilter.getItemSkuKeyword().toUpperCase())
          .append(SolrConstants.CLOSING_BRACKET);
    }
    //Moving offer price from fq to q
    if (Objects.nonNull(itemFilter.getOfferPrice())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.OFFER_PRICE).append(SolrConstants.COLON)
          .append(appendDoubleQuotes(String
              .format(SolrConstants.OFFER_PRICE_FORMATTER, channel, this.solrStringDelimiter,
                  itemFilter.getOfferPrice())));
    }
    if (StringUtils.isNotBlank(itemFilter.getProductItemName())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.ITEM_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(itemFilter.getProductItemName().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.MM, 100);
    }
    if (StringUtils.isNotBlank(itemFilter.getSalesCategoryCode())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.SALES_CATALOG)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(itemFilter.getSalesCategoryCode())
          .append(SolrConstants.CLOSING_BRACKET);
    }
    if (Objects.nonNull(itemFilter.getIsTradingProduct())) {
      solrQuery.addFilterQuery(SolrFieldNames.TRADING_PRODUCT + SolrConstants.COLON + itemFilter.getIsTradingProduct());
    }
    if (itemFilter.isCncActivated()) {
      solrQuery.addFilterQuery(SolrFieldNames.CNC_ACTIVE + SolrConstants.COLON + Boolean.TRUE);
    }
    if (StringUtils.isNotBlank(itemFilter.getPickupPointCode())) {
      if (itemFilter.isCncActivated()) {
        defaultQuery.append(addPickupPointCodeInSolrQuery(itemFilter.getPickupPointCode()));
      } else {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(
            SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON + appendDoubleQuotes(
                itemFilter.getPickupPointCode()));
      }
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getPickupPointCodes())) {
      List<String> pickupCodes =
          itemFilter.getPickupPointCodes().stream().filter(StringUtils::isNotBlank).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(pickupCodes)) {
        if (itemFilter.isCncActivated()) {
          defaultQuery.append(addPickupPointCodesInSolrQuery(pickupCodes));
        } else {
          defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.PICKUP_POINT_CODE)
              .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
              .append(String.join(SolrConstants.OR_CLAUSE, pickupCodes))
              .append(SolrConstants.CLOSING_BRACKET);
        }
      }
    }
    if (StringUtils.isNotBlank(itemFilter.getSearchKey())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(itemFilter.getSearchKey().trim())).append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.ITEM_NAME + StringUtils.SPACE + SolrFieldNames.ITEM_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }
    solrQuery.setQuery(defaultQuery.toString());
    return solrQuery;
  }

  private SolrQuery constructSolrQueryForBulkItemCodes(String storeId, BulkItemSummaryRequestVo itemFilter,
      SolrQuery solrQuery) {
    if (StringUtils.isNotBlank(itemFilter.getMerchantSku())) {
      if (CollectionUtils.isNotEmpty(itemFilter.getMerchantSkus())) {
        itemFilter.getMerchantSkus().add(itemFilter.getMerchantSku());
      } else {
        itemFilter.setMerchantSkus(Collections.singletonList(itemFilter.getMerchantSku()));
      }
    }
    StringBuilder defaultQuery = new StringBuilder(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    if (StringUtils.isNotBlank(itemFilter.getProductCode())) {
      if (isQueryHasStoreId(defaultQuery.toString(), storeId)) {
        defaultQuery = new StringBuilder(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + itemFilter.getProductCode()) ;
        solrQuery.addFilterQuery(getStoreIdQuery(storeId));
      } else {
        solrQuery.addFilterQuery(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + itemFilter.getProductCode());
      }
    }
    if (StringUtils.isNotBlank(itemFilter.getMerchantCode())) {
      if (isQueryHasStoreId(defaultQuery.toString(), storeId)) {
        defaultQuery = new StringBuilder(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + itemFilter.getMerchantCode()) ;
        solrQuery.addFilterQuery(getStoreIdQuery(storeId));
      } else {
        solrQuery.addFilterQuery(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + itemFilter.getMerchantCode());
      }
    }
    if (Objects.nonNull(itemFilter.getArchived())) {
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + itemFilter.getArchived());
    }
    String channel = this.channelDefaultValue;
    if (StringUtils.isNotBlank(itemFilter.getChannelName())) {
      channel = itemFilter.getChannelName();
    }
    if (Objects.nonNull(itemFilter.getBuyable())) {
      solrQuery.addFilterQuery(SolrFieldNames.BUYABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, itemFilter.getBuyable()));
    }
    if (Objects.nonNull(itemFilter.getDiscoverable())) {
      solrQuery.addFilterQuery(SolrFieldNames.DISCOVERABLE + SolrConstants.COLON + String
          .format(SolrConstants.STRING_FORMATTER, channel, this.solrStringDelimiter, itemFilter.getDiscoverable()));
    }
    if (StringUtils.isNotBlank(itemFilter.getMasterCategoryCode())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.MASTER_CATALOG)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(itemFilter.getMasterCategoryCode())
          .append(SolrConstants.CLOSING_BRACKET);
    }
    if (StringUtils.isNotBlank(itemFilter.getItemSkuKeyword())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.ITEM_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(itemFilter.getItemSkuKeyword().toUpperCase())
          .append(SolrConstants.CLOSING_BRACKET);
    }
    //Move from fq to q
    if (Objects.nonNull(itemFilter.getOfferPrice())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.OFFER_PRICE).append(SolrConstants.COLON)
          .append(appendDoubleQuotes(String
              .format(SolrConstants.OFFER_PRICE_FORMATTER, channel, this.solrStringDelimiter,
                  itemFilter.getOfferPrice())));
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getItemSkus())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.ITEM_SKU + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getItemSkus()));
    }

    if (CollectionUtils.isNotEmpty(itemFilter.getExcludedItemSkus())) {
      List<String> excludedItems = itemFilter.getExcludedItemSkus().stream()
        .filter(StringUtils::isNotBlank)
        .collect(Collectors.toList());

      if (!excludedItems.isEmpty()) {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.NOT_IN)
          .append(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + getFQForListOfStrings(excludedItems));
      }
    }

    if (StringUtils.isNotBlank(itemFilter.getLinkedPartnerCode())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.NOT_IN)
        .append(SolrFieldNames.LINKED_PARTNERS + SolrConstants.COLON + getFQForListOfStrings(
          Arrays.asList(itemFilter.getLinkedPartnerCode())));
    }

    if (StringUtils.isNotBlank(itemFilter.getProductItemName())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.ITEM_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(itemFilter.getProductItemName().trim()))
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.MM, 100);
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getItemCodes()) || CollectionUtils.isNotEmpty(itemFilter.getPristineIds())) {
      if (CollectionUtils.isEmpty(itemFilter.getPristineIds())) {
        solrQuery.addFilterQuery(
            SolrFieldNames.ITEM_CODE + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getItemCodes()));
      } else if (CollectionUtils.isEmpty(itemFilter.getItemCodes())) {
        solrQuery.addFilterQuery(
            SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getPristineIds()));
      } else {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrConstants.OPEN_BRACKET)
            .append(SolrFieldNames.ITEM_CODE + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getItemCodes()))
            .append(SolrConstants.OR_CLAUSE).append(
            SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getPristineIds()))
            .append(SolrConstants.CLOSING_BRACKET);
      }
    }
    if (StringUtils.isNotBlank(itemFilter.getSalesCategoryCode())) {
      defaultQuery.append(SolrConstants.AND_CLAUSE).append(SolrFieldNames.SALES_CATALOG)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(itemFilter.getSalesCategoryCode())
          .append(SolrConstants.CLOSING_BRACKET);
    }
    if (CollectionUtils.isNotEmpty(itemFilter.getMerchantSkus())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.MERCHANT_SKU + SolrConstants.COLON + getFQForListOfStrings(itemFilter.getMerchantSkus()));
    }
    if (Objects.nonNull(itemFilter.getIsTradingProduct())) {
      solrQuery.addFilterQuery(SolrFieldNames.TRADING_PRODUCT + SolrConstants.COLON + itemFilter.getIsTradingProduct());
    }
    if (itemFilter.isCncActivated()) {
      solrQuery.addFilterQuery(SolrFieldNames.CNC_ACTIVE + SolrConstants.COLON + Boolean.TRUE);
    }
    if (StringUtils.isNotBlank(itemFilter.getPickupPointCode())) {
      if (itemFilter.isCncActivated()) {
        defaultQuery.append(addPickupPointCodeInSolrQuery(itemFilter.getPickupPointCode()));
      } else {
        defaultQuery.append(SolrConstants.AND_CLAUSE).append(
            SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON + appendDoubleQuotes(
                itemFilter.getPickupPointCode()));
      }
    }
    solrQuery.setQuery(defaultQuery.toString());
    return solrQuery;
  }

  private SolrQuery addGroupQuery(Pageable page, SolrQuery solrQuery) {
    solrQuery.set(GroupParams.GROUP, Boolean.TRUE);
    solrQuery.set(GroupParams.GROUP_FIELD, SolrFieldNames.PRODUCT_SKU);
    solrQuery.set(GroupParams.GROUP_MAIN, Boolean.TRUE);
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.SALES_CATALOG);
    solrQuery.setStart(page.getPageNumber() * page.getPageSize());
    solrQuery.setRows(page.getPageSize());
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    return solrQuery;
  }

  private SolrQuery addFieldsForGetProductsWithNullSalesCatalog(Pageable page, SolrQuery solrQuery) {
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.SALES_CATALOG);
    solrQuery.setStart(page.getPageNumber() * page.getPageSize());
    solrQuery.setRows(page.getPageSize());
    solrQuery.setSort(SolrFieldNames.PRODUCT_SKU, SolrQuery.ORDER.asc);
    return solrQuery;
  }

  private SolrQuery generateGroupQueryOfficialStore(Pageable pageable, SolrQuery solrQuery) {
    solrQuery.setFields(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_NAME, SolrFieldNames.MASTER_CATALOG,
        SolrFieldNames.SALES_CATALOG, SolrFieldNames.ITEM_SKU, SolrFieldNames.PRODUCT_CODE, SolrFieldNames.OFFER_PRICE,
        SolrFieldNames.LIST_PRICE, SolrFieldNames.ITEM_IMAGES, SolrFieldNames.MERCHANT_CODE, SolrFieldNames.BRAND);
    solrQuery.set(SolrConstants.GROUP, Boolean.TRUE);
    solrQuery.set(GroupParams.GROUP_FIELD, SolrFieldNames.PRODUCT_SKU);
    solrQuery.set(GroupParams.GROUP_TOTAL_COUNT, Boolean.TRUE);
    solrQuery.set(GroupParams.GROUP_LIMIT, solrGroupLimitForProductSku);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.setSort(SolrFieldNames.ID, SolrQuery.ORDER.asc);
    return solrQuery;
  }

  private String getStoreIdQuery(String storeId) {
    return SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId;
  }

  private boolean isQueryHasStoreId(String query, String storeId) {
    return StringUtils.equals(query, getStoreIdQuery(storeId));
  }

  private void checkStoreIdQueryAndSetQuery(String storeId, SolrQuery solrQuery, String subQuery, boolean isSetQuery) {
    if (isQueryHasStoreId(solrQuery.getQuery(), storeId)) {
      solrQuery.setQuery(subQuery);
      solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    } else {
      if (!isSetQuery) {
        solrQuery.addFilterQuery(subQuery);
      } else {
        solrQuery.setQuery(solrQuery.getQuery() + SolrConstants.AND_CLAUSE + subQuery);
      }
    }
  }

  private void checkStoreIdQueryAndSetFilterQuery(String storeId, SolrQuery solrQuery) {
    if(!isQueryHasStoreId(solrQuery.getQuery(), storeId)) {
      solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    }
  }

  private SolrQuery constructSolrQueryForSuspension(String storeId, ActiveProductsRequestVO activeProductsRequestVO,
      boolean productLevelSearch) {
    log.info("Constructing solr for suspension for request: {} and storeId : {} ", activeProductsRequestVO, storeId);
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    StringBuilder query = new StringBuilder();
    if (ALL.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      query.append(SolrConstants.OPEN_BRACKET).append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MARK_FOR_DELETE)
          .append(SolrConstants.COLON).append(Boolean.FALSE).append(SolrConstants.CLOSING_BRACKET)
          .append(SolrConstants.OR_CLAUSE).append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.IS_SUSPENDED)
          .append(SolrConstants.COLON).append(Boolean.TRUE).append(SolrConstants.CLOSING_BRACKET)
          .append(SolrConstants.CLOSING_BRACKET);
    } else if (ACTIVE.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MARK_FOR_DELETE).append(SolrConstants.COLON)
          .append(Boolean.FALSE).append(SolrConstants.CLOSING_BRACKET);
    } else if (SUSPENDED.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.IS_SUSPENDED).append(SolrConstants.COLON)
          .append(Boolean.TRUE).append(SolrConstants.CLOSING_BRACKET);
    }

    if (StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode())) {
      if (!StringUtils.isBlank(query)) {
        query.append(SolrConstants.AND_CLAUSE);
      }
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
          .append(activeProductsRequestVO.getMerchantCode()).append(SolrConstants.CLOSING_BRACKET);
    }

    if (StringUtils.isNotBlank(activeProductsRequestVO.getNameKey())) {
      if (!StringUtils.isBlank(query)) {
        query.append(SolrConstants.AND_CLAUSE);
      }
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.ITEM_NAME).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET)
          .append(ClientUtils.escapeQueryChars(activeProductsRequestVO.getNameKey().trim()))
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.MM, 100);
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getCategoryCodes())) {
      if (!StringUtils.isBlank(query)) {
        query.append(SolrConstants.AND_CLAUSE);
      }
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
          .append(SolrConstants.OPEN_BRACKET).append(getFQForListOfStrings(activeProductsRequestVO.getCategoryCodes()))
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET);
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getPickupPointCodes())) {
      if (!StringUtils.isBlank(query)) {
        query.append(SolrConstants.AND_CLAUSE);
      }
      List<String> pickupCodes = activeProductsRequestVO.getPickupPointCodes().stream().filter(StringUtils::isNotBlank)
          .collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(pickupCodes)) {
        query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.PICKUP_POINT_CODE).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(getFQForListOfStrings(activeProductsRequestVO.getPickupPointCodes()))
            .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET);
      }
    }

    if (!productLevelSearch && StringUtils.isNotBlank(activeProductsRequestVO.getSearchKey())) {
      if (!StringUtils.isBlank(query)) {
        query.append(SolrConstants.AND_CLAUSE);
      }
      query.append(SolrConstants.OPEN_BRACKET).append(activeProductsRequestVO.getSearchKey().trim())
          .append(SolrConstants.CLOSING_BRACKET);
      solrQuery.set(SolrConstants.QF, SolrFieldNames.ITEM_NAME + StringUtils.SPACE + SolrFieldNames.ITEM_SKU);
      solrQuery.set(SolrConstants.MM, 100);
    }

    solrQuery.setQuery(query.toString());
    log.info("Solr for suspension for request: {} and is : {} ", activeProductsRequestVO, solrQuery.getQuery());

    if (StringUtils.isBlank(activeProductsRequestVO.getSortType())) {
      solrQuery.setSort(SolrFieldNames.UPDATED_DATE, SolrQuery.ORDER.desc);
    } else if (SolrQuery.ORDER.asc.toString().equals(activeProductsRequestVO.getSortType().toLowerCase())) {
      solrQuery.setSort(SolrFieldNames.UPDATED_DATE, SolrQuery.ORDER.asc);
    } else if (SolrQuery.ORDER.desc.toString().equals(activeProductsRequestVO.getSortType().toLowerCase())) {
      solrQuery.setSort(SolrFieldNames.UPDATED_DATE, SolrQuery.ORDER.desc);
    }
    return solrQuery;
  }

  private SolrQuery constructSolrQueryForProductSuspension(String storeId, ActiveProductsRequestVO activeProductsRequestVO) {
    log.info("Constructing solr for product suspension for request: {} and storeId : {} ", activeProductsRequestVO,
        storeId);
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    if (!(Boolean.valueOf(systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION).getValue()))) {
      solrQuery.addFilterQuery(COLLAPSE_QUERY_FOR_PRODUCT_SKU);
    }

    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode())) {
      query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
          .append(activeProductsRequestVO.getMerchantCode()).append(SolrConstants.CLOSING_BRACKET);
    }

    if (CollectionUtils.isNotEmpty(activeProductsRequestVO.getCategoryCodes())) {
      if (StringUtils.isBlank(query)) {
        query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET)
            .append(getFQForListOfStrings(activeProductsRequestVO.getCategoryCodes()))
            .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET);
      } else {
        solrQuery.addFilterQuery(
            SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + getFQForListOfStrings(
                activeProductsRequestVO.getCategoryCodes()) + SolrConstants.CLOSING_BRACKET);
      }
    }
    if (ALL.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.MARK_FOR_DELETE).append(SolrConstants.COLON).append(Boolean.FALSE)
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.OR_CLAUSE).append(SolrConstants.OPEN_BRACKET)
          .append(SolrFieldNames.IS_SUSPENDED).append(SolrConstants.COLON).append(Boolean.TRUE)
          .append(SolrConstants.CLOSING_BRACKET).append(SolrConstants.CLOSING_BRACKET).toString());
    } else if (ACTIVE.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.MARK_FOR_DELETE)
              .append(SolrConstants.COLON).append(Boolean.FALSE).append(SolrConstants.CLOSING_BRACKET).toString());
    } else if (SUSPENDED.equalsIgnoreCase(activeProductsRequestVO.getStatus())) {
      solrQuery.addFilterQuery(
          new StringBuilder().append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.IS_SUSPENDED)
              .append(SolrConstants.COLON).append(Boolean.TRUE).append(SolrConstants.CLOSING_BRACKET).toString());
    }

    if (StringUtils.isNotBlank(activeProductsRequestVO.getSearchKey())) {
      query.append(SolrConstants.OPEN_BRACKET + SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON
          + SolrConstants.DOUBLE_QUOTE).append(activeProductsRequestVO.getSearchKey().trim())
        .append(SolrConstants.DOUBLE_QUOTE).append(SolrConstants.OR_CLAUSE)
        .append(SolrFieldNames.PRODUCT_CODE).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTE).append(activeProductsRequestVO.getSearchKey().trim())
        .append(SolrConstants.DOUBLE_QUOTE).append(SolrConstants.OR_CLAUSE)
        .append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTE).append(activeProductsRequestVO.getSearchKey().trim())
        .append(SolrConstants.DOUBLE_QUOTE).append(SolrConstants.CLOSING_BRACKET);
    }
    if (StringUtils.isEmpty(query.toString())) {
      query.append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
          .append(SolrConstants.SOLR_EXPRESSION_DELIMITER);
    }
    solrQuery.setSort(SolrFieldNames.UPDATED_DATE, SolrQuery.ORDER.asc);
    solrQuery.setQuery(query.toString());
    log.info("Solr for suspension for request: {} and is : {} ", activeProductsRequestVO, solrQuery.getQuery());
    return solrQuery;
  }

  @Override
  public Long findByStoreIdAndBrandAndMarkForDeleteFalse(String storeId, String brand) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery
        .setQuery(SolrFieldNames.BRAND + SolrConstants.COLON + SolrConstants.QUOTES + brand + SolrConstants.QUOTES);
    solrQuery.addFilterQuery(getStoreIdQuery(storeId));
    solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    try {
      QueryResponse queryResponse = this.cloudSolrClientL3.query(solrQuery);
      return queryResponse.getResults().getNumFound();
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception caught while fetching solr documents by brand : {} ", brand, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  public Long getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(String storeId,
      List<List<String>> categoryCodesInBatches, List<String> brands, String merchantCode) {
    Long totalProducts = 0L;
    for (List<String> categoryCodes : categoryCodesInBatches) {
      SolrQuery solrQuery = constructCategoryBrandMerchantCodeQuery(
          categoryCodes, brands, merchantCode);
      solrQuery.addFilterQuery(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE);
      solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
      solrQuery.addFilterQuery(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE);
      solrQuery.setParam(SolrFieldNames.JSON_FACET, SolrFieldNames.SUM_OF_L5_COUNT);
      solrQuery.setRows(Constants.SINGLE_ROW);
      try {
        QueryResponse queryResponse = this.cloudSolrClientL3.query(solrQuery);
        if (Objects.nonNull(queryResponse.getJsonFacetingResponse().getStatValue(SolrFieldNames.L5_COUNT))) {
          totalProducts = totalProducts + ((Double) queryResponse.getJsonFacetingResponse()
              .getStatValue(SolrFieldNames.L5_COUNT)).longValue();
        }
      } catch (SolrServerException | SolrException | IOException e) {
        log.error("Exception caught while fetching solr product l3 documents by"
            + " category codes : {} and brands : {}", categoryCodes, brands, e);
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
    return totalProducts;
  }

  private SolrQuery constructCategoryBrandMerchantCodeQuery(
      List<String> categoryCodes, List<String> brands, String merchantCode){
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder solrQueryStringBuilder = new StringBuilder();
    if(CollectionUtils.isNotEmpty(categoryCodes)) {
      solrQueryStringBuilder.append(SolrFieldNames.MASTER_CATALOG)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, categoryCodes)).append(SolrConstants.CLOSING_BRACKET);
      solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE);
    }
    if(CollectionUtils.isNotEmpty(brands)) {
      solrQueryStringBuilder.append(SolrFieldNames.BRAND)
          .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(String.join(SolrConstants.OR_CLAUSE, brands)).append(SolrConstants.CLOSING_BRACKET);
      solrQueryStringBuilder.append(SolrConstants.AND_CLAUSE);
    }
    solrQueryStringBuilder.append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON).append(merchantCode);
    solrQuery.setQuery(solrQueryStringBuilder.toString());
    return solrQuery;
  }

}
