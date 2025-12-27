package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gda.mta.product.dto.HistoryRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SolrHistoryCollectionRepositoryBeanTest {

  @InjectMocks
  private SolrHistoryCollectionRepositoryBean solrHistoryCollectionRepositoryBean;

  @Mock
  @Qualifier(value = "historyCollectionClient")
  private CloudSolrClient historyCollectionClient;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private UpdateResponse updateResponse;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> solrQueryArgumentCaptor1;

  private Long AUDIT_TRAIL_ID = 123L;
  private Long AUDIT_TRAIL_ID1 = 126L;
  private String RESULT = "id:(\"123\",\"126\")";
  private static final String PRODUCT_SKU = "PRODUCT-SKU";
  private static final String ITEM_SKU = "ITEM-SKU";
  private static final String ITEM_NAME = "ITEM NAME VARIANT 1";
  private static final String QUERY_1 = "(product_sku:\"PRODUCT-SKU\")";
  private static final String QUERY_2 =
      "(product_sku:\"PRODUCT-SKU\" AND (gdn_sku:\"ITEM-SKU\" OR gdn_sku:\"DEFAULT\" OR gdn_name:(\"ITEM-SKU\") OR activity:\"ITEM-SKU\"))";
  private static final String QUERY_3 =
      "(product_sku:\"PRODUCT-SKU\" AND (gdn_sku:\"ITEM NAME VARIANT 1\" OR gdn_sku:\"DEFAULT\" OR gdn_name:(\"ITEM\" AND \"NAME\" AND \"VARIANT\" AND \"1\") OR activity:\"ITEM NAME VARIANT 1\"))";
  private static final String STORE_ID = "storeId";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String KEYWORD = "keyword";

  private HistoryRequest historyRequest;
  private HistoryUpdateRequest historyUpdateRequest;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    historyRequest = new HistoryRequest(PRODUCT_SKU, null, ITEM_SKU, new Date(), new Date(), false);
    historyUpdateRequest =
      HistoryUpdateRequest.builder().beforeOneMonths(true).pickupPointCode(PICKUP_POINT_CODE)
        .productSku(PRODUCT_SKU).keyword(KEYWORD).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(historyCollectionClient);
  }

  @Test
  public void findVariantHistoryByProductSkuAndKeywordTest() throws IOException, SolrServerException {
    historyRequest.setKeyword(null);
    historyRequest.setEndDate(null);
    historyRequest.setStartDate(null);
    when(this.historyCollectionClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(new SolrDocumentList());
    this.solrHistoryCollectionRepositoryBean
        .findProductHistoryByProductSkuAndKeyword(Constants.DEFAULT_STORE_ID, historyRequest, 0, 10);
    verify(this.historyCollectionClient).query(solrQueryArgumentCaptor.getValue());
    verify(this.queryResponse).getResults();
    Assertions.assertEquals(QUERY_1, solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertEquals(0, solrQueryArgumentCaptor.getValue().getStart().intValue());
    Assertions.assertEquals(10, solrQueryArgumentCaptor.getValue().getRows().intValue());
    Assertions.assertEquals(SolrFieldNames.ACCESS_TIME + " " + SolrQuery.ORDER.desc,
        solrQueryArgumentCaptor.getValue().getSortField());
    Assertions.assertEquals(1, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(SolrConstants.NOT_PREDICATE + SolrFieldNames.ONLINE_STATUS + SolrConstants.COLON
            + Boolean.FALSE, solrQueryArgumentCaptor.getValue().getFilterQueries()[0]);
  }

  @Test
  public void findVariantHistoryByProductSkuAndKeywordWithValueTest() throws IOException, SolrServerException {
    when(this.historyCollectionClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(new SolrDocumentList());
    this.solrHistoryCollectionRepositoryBean
        .findProductHistoryByProductSkuAndKeyword(Constants.DEFAULT_STORE_ID, historyRequest, 0, 50);
    verify(this.historyCollectionClient).query(solrQueryArgumentCaptor.getValue());
    verify(this.queryResponse).getResults();
    Assertions.assertEquals(QUERY_2, solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertEquals(0, solrQueryArgumentCaptor.getValue().getStart().intValue());
    Assertions.assertEquals(50, solrQueryArgumentCaptor.getValue().getRows().intValue());
    System.out.println(solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertEquals(SolrFieldNames.ACCESS_TIME + " " + SolrQuery.ORDER.desc,
        solrQueryArgumentCaptor.getValue().getSortField());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue().getFilterQueries());
  }

  @Test
  public void findVariantHistoryByProductSkuAndKeywordWithNameTest() throws IOException, SolrServerException {
    historyRequest.setKeyword(ITEM_NAME);
    when(this.historyCollectionClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(new SolrDocumentList());
    this.solrHistoryCollectionRepositoryBean
        .findProductHistoryByProductSkuAndKeyword(Constants.DEFAULT_STORE_ID, historyRequest, 0, 50);
    verify(this.historyCollectionClient).query(solrQueryArgumentCaptor.getValue());
    verify(this.queryResponse).getResults();
    Assertions.assertEquals(QUERY_3, solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertEquals(0, solrQueryArgumentCaptor.getValue().getStart().intValue());
    Assertions.assertEquals(50, solrQueryArgumentCaptor.getValue().getRows().intValue());
    System.out.println(solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertEquals(SolrFieldNames.ACCESS_TIME + " " + SolrQuery.ORDER.desc,
        solrQueryArgumentCaptor.getValue().getSortField());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue().getFilterQueries());
  }


  @Test
  public void addDocument_EmptyTest() throws IOException, SolrServerException {
    when(historyCollectionClient.add(Mockito.any(List.class))).thenReturn(updateResponse);
    solrHistoryCollectionRepositoryBean.addDocument(new ArrayList<>());
  }

  @Test
  public void addDocumentTest() throws IOException, SolrServerException {
    when(historyCollectionClient.add(Mockito.any(List.class))).thenReturn(updateResponse);
    solrHistoryCollectionRepositoryBean.addDocument(Arrays.asList(Mockito.any(SolrInputDocument.class)));
    verify(historyCollectionClient).add(Mockito.any(List.class));
  }

  @Test
  public void addDocumentErrorTest() throws IOException, SolrServerException {
    when(historyCollectionClient.add(Mockito.any(List.class))).thenThrow(SolrServerException.class);
    solrHistoryCollectionRepositoryBean.addDocument(Arrays.asList(Mockito.any(SolrInputDocument.class)));
    verify(historyCollectionClient).add(Mockito.any(List.class));
  }

  @Test
  public void deleteHistoryFromSolrTest() throws Exception {
    when(historyCollectionClient.deleteByQuery(Mockito.anyString())).thenReturn(new UpdateResponse());
    solrHistoryCollectionRepositoryBean.deleteHistoryFromSolr(12, new Date());
    Mockito.verify(this.historyCollectionClient).deleteByQuery(solrQueryArgumentCaptor1.capture());
  }


  public void deleteHistoryFromSolr_ExceptionTest() throws Exception {
    when(historyCollectionClient.deleteByQuery(anyString())).thenThrow(Exception.class);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        solrHistoryCollectionRepositoryBean.deleteHistoryFromSolr(12, new Date());
      });
    } finally {
      Mockito.verify(this.historyCollectionClient).deleteByQuery(anyString());
    }
  }

  @Test
  public void findProductUpdateHistoryByRequestTest() throws Exception {
    when(historyCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(
      new QueryResponse());
    solrHistoryCollectionRepositoryBean.findProductUpdateHistoryByRequest(STORE_ID,
        historyUpdateRequest, PAGE, SIZE);
    verify(historyCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(PRODUCT_SKU));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(PICKUP_POINT_CODE));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(KEYWORD));
  }

  @Test
  public void findProductUpdateHistoryByRequest_emptyRequestTest() throws Exception {
    when(historyCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(
      new QueryResponse());
    solrHistoryCollectionRepositoryBean.findProductUpdateHistoryByRequest(STORE_ID,
      new HistoryUpdateRequest(), PAGE, SIZE);
    verify(historyCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue());
    Assertions.assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(PRODUCT_SKU));
    Assertions.assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(PICKUP_POINT_CODE));
    Assertions.assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(KEYWORD));
  }

  @Test
  public void findProductUpdateHistoryByRequest_onlyPickupPointCodeTest() throws Exception {
    historyUpdateRequest.setProductSku(null);
    when(historyCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(
      new QueryResponse());
    solrHistoryCollectionRepositoryBean.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE);
    verify(historyCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue());
    Assertions.assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(PRODUCT_SKU));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(PICKUP_POINT_CODE));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(KEYWORD));
  }

  @Test
  public void findProductUpdateHistoryByRequest_onlyPickupPointCodeWithDateRangeTest() throws Exception {
    historyUpdateRequest.setProductSku(null);
    historyUpdateRequest.setStartDate(new Date());
    historyUpdateRequest.setEndDate(new Date());
    when(historyCollectionClient.query(Mockito.any(SolrQuery.class))).thenReturn(
        new QueryResponse());
    solrHistoryCollectionRepositoryBean.findProductUpdateHistoryByRequest(STORE_ID,
        historyUpdateRequest, PAGE, SIZE);
    verify(historyCollectionClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertNotNull(solrQueryArgumentCaptor.getValue());
    Assertions.assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(PRODUCT_SKU));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(PICKUP_POINT_CODE));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(KEYWORD));
  }

}
