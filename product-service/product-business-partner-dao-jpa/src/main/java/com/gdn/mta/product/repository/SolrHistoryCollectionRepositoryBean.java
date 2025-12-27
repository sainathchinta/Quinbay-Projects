package com.gdn.mta.product.repository;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Repository;

import com.gda.mta.product.dto.HistoryRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class SolrHistoryCollectionRepositoryBean implements SolrHistoryCollectionRepository {

  @Autowired
  @Qualifier(value = "historyCollectionClient")
  private CloudSolrClient historyCollectionClient;

  @Override
  @Async
  public void addDocument(List<SolrInputDocument> solrDocumentList) {
    try {
      if (CollectionUtils.isNotEmpty(solrDocumentList)) {
        historyCollectionClient.add(solrDocumentList);
      }
    } catch (SolrServerException | SolrException | IOException e) {
      log.error("Exception while adding document to solr  {} ", e);
    }
  }

  @Override
  public SolrDocumentList findProductHistoryByProductSkuAndKeyword(String storeId, HistoryRequest historyRequest,
      int page, int size) throws SolrServerException, IOException {
    SolrQuery solrQuery = getSolrQueryForHistory(historyRequest, page, size);
    QueryResponse queryResponse = historyCollectionClient.query(solrQuery);
    return queryResponse.getResults();
  }

  private SolrQuery getSolrQueryForHistory(HistoryRequest historyRequest, int page, int size) {
    SolrQuery solrQuery = new SolrQuery();
    //productSku will be mandatory field for history listing
    StringBuilder query = new StringBuilder(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + historyRequest.getProductSku()
            + SolrConstants.DOUBLE_QUOTES);
    if (StringUtils.isNotEmpty(historyRequest.getKeyword())) {
      query.append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE)
          .append(SolrConstants.OPEN_BRACKET).append(
          SolrFieldNames.GDN_SKU + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + historyRequest.getKeyword().trim()
              + SolrConstants.DOUBLE_QUOTES).append(StringUtils.SPACE).append(SolrConstants.OR)
          .append(StringUtils.SPACE).append(
          SolrFieldNames.GDN_SKU + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + Constants.DEFAULT
              + SolrConstants.DOUBLE_QUOTES).append(StringUtils.SPACE).append(SolrConstants.OR)
          .append(StringUtils.SPACE).append(getKeyWordSearchQuery(historyRequest.getKeyword().trim())).append(SolrConstants.CLOSE_BRACKET);
    }
    solrQuery.setQuery(SolrConstants.OPEN_BRACKET + query.toString() + SolrConstants.CLOSE_BRACKET);
    if (!(Objects.isNull(historyRequest.getStartDate()) && Objects.isNull(historyRequest.getEndDate()))) {
      solrQuery.setFilterQueries(getTimeFilter(historyRequest.getStartDate(), historyRequest.getEndDate()));
    }
    solrQuery.addFilterQuery(SolrConstants.NOT_PREDICATE + SolrFieldNames.ONLINE_STATUS + SolrConstants.COLON + Boolean.FALSE);
    solrQuery.setSort(SolrFieldNames.ACCESS_TIME, SolrQuery.ORDER.desc);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    return solrQuery;

  }

  private String getKeyWordSearchQuery(String keyword) {
    String[] keywordTokens = keyword.split(StringUtils.SPACE);
    for (int i = 0; i < keywordTokens.length; i++) {
      keywordTokens[i] = SolrConstants.DOUBLE_QUOTES + keywordTokens[i] + SolrConstants.DOUBLE_QUOTES;
    }
    StringBuilder defaultQuery = new StringBuilder();
    defaultQuery.append(SolrFieldNames.GDN_NAME).append(Constants.COLON).append(SolrConstants.OPEN_BRACKET)
        .append(String.join(StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE, keywordTokens))
        .append(SolrConstants.CLOSE_BRACKET);
    defaultQuery.append(StringUtils.SPACE).append(SolrConstants.OR).append(StringUtils.SPACE)
        .append(SolrFieldNames.ACTIVITY).append(Constants.COLON).append(SolrConstants.DOUBLE_QUOTES)
        .append(keyword.trim()).append(SolrConstants.DOUBLE_QUOTES);
    return defaultQuery.toString();
  }

  private String getTimeFilter(Date startDate, Date endDate) {
    StringBuilder defaultQuery = new StringBuilder();
    if (Objects.isNull(endDate)) {
      endDate = new Date(); //set endDate as current timestamp.
    } else {
      endDate = Date.from(endDate.toInstant().plus(Constants.ONE_DAY, ChronoUnit.DAYS));
    }
    try {
      defaultQuery.append(SolrFieldNames.ACCESS_TIME + SolrConstants.COLON + SolrConstants.OPEN_SQUARE_BRACKET
          + SolrConstants.DOUBLE_QUOTES + CommonUtils.toJakartaTimeZone(startDate) + SolrConstants.DOUBLE_QUOTES
          + StringUtils.SPACE + SolrConstants.TO + StringUtils.SPACE + SolrConstants.DOUBLE_QUOTES + CommonUtils
          .toJakartaTimeZone(endDate) + SolrConstants.DOUBLE_QUOTES + SolrConstants.CLOSE_SQUARE_BRACKET);
    } catch (UnsupportedEncodingException | ParseException e) {
      log.error("Error while parsing date. startDate : {}, endDate : {} ", startDate, endDate, e);
    }
    return defaultQuery.toString();
  }

  @Override
  public void deleteHistoryFromSolr(int days, Date accessTime) throws Exception {
    try {
      historyCollectionClient.deleteByQuery(
          SolrFieldNames.ACCESS_TIME + SolrConstants.COLON + SolrConstants.OPEN_SQUARE_BRACKET
              + SolrConstants.LIKE_QUERY + StringUtils.SPACE + SolrConstants.TO + StringUtils.SPACE + SolrConstants.NOW
              + SolrConstants.NOT + days + SolrConstants.DAYS + SolrConstants.CLOSE_SQUARE_BRACKET);
    } catch (Exception e) {
      log.error("Error deleting documents by accessTime : {} ", accessTime, e);
      throw e;
    }
  }

  @Override
  public SolrDocumentList findProductUpdateHistoryByRequest(String storeId, HistoryUpdateRequest historyUpdateRequest, int page, int size)
    throws SolrServerException, IOException {
    SolrQuery solrQuery = getSolrQueryForUpdateHistory(historyUpdateRequest, page, size);
    QueryResponse queryResponse = historyCollectionClient.query(solrQuery);
    return queryResponse.getResults();
  }

  private SolrQuery getSolrQueryForUpdateHistory(HistoryUpdateRequest historyUpdateRequest, int page, int size) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotEmpty(historyUpdateRequest.getProductSku())) {
      query.append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTES).append(historyUpdateRequest.getProductSku())
        .append(SolrConstants.DOUBLE_QUOTES);
    }
    if (StringUtils.isNotEmpty(historyUpdateRequest.getPickupPointCode())) {
      if (StringUtils.isNotEmpty(query)) {
        query.append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE);
      }
      query.append(SolrFieldNames.PICKUP_POINT_CODE).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTES).append(historyUpdateRequest.getPickupPointCode())
        .append(SolrConstants.DOUBLE_QUOTES);
    }
    if (StringUtils.isNotEmpty(historyUpdateRequest.getKeyword())) {
      query.append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE)
        .append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.GDN_SKU).append(SolrConstants.COLON)
        .append(SolrConstants.DOUBLE_QUOTES).append(historyUpdateRequest.getKeyword().trim()).append(SolrConstants.DOUBLE_QUOTES)
        .append(StringUtils.SPACE).append(SolrConstants.OR).append(StringUtils.SPACE)
        .append(SolrFieldNames.GDN_SKU).append(SolrConstants.COLON).append(SolrConstants.DOUBLE_QUOTES)
        .append(Constants.DEFAULT).append(SolrConstants.DOUBLE_QUOTES)
        .append(StringUtils.SPACE).append(SolrConstants.OR).append(StringUtils.SPACE)
        .append(getKeyWordSearchQuery(historyUpdateRequest.getKeyword().trim()))
        .append(SolrConstants.CLOSE_BRACKET);
    }
    if (Objects.nonNull(historyUpdateRequest.getStartDate()) || Objects.nonNull(
      historyUpdateRequest.getEndDate())) {
      query.append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE)
          .append(getTimeFilter(historyUpdateRequest.getStartDate(), historyUpdateRequest.getEndDate()));
    }
    solrQuery.setQuery(SolrConstants.OPEN_BRACKET + query + SolrConstants.CLOSE_BRACKET);
    solrQuery.setSort(SolrFieldNames.ACCESS_TIME, SolrQuery.ORDER.desc);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    return solrQuery;
  }
}
