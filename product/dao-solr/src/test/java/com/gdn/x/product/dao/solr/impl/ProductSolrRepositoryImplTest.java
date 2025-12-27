package com.gdn.x.product.dao.solr.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.model.vo.ReelProductListingRequestVo;
import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.util.NamedList;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.model.vo.ProductSkuSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.service.config.KafkaPublisher;

public class ProductSolrRepositoryImplTest {

  private static final String PRODUCT_SKU = "product_sku";
  private static final String PRODUCT_NAME = "product_name";
  private static final String PRODUCT_CODE = "product_code";
  private static final String MERCHANT_CODE = "merchant_code";
  private static final String MERCHANT_CODE1 = "merchant_code_1";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String MERCHANT_CODE_WITH_QUOTES = "\"merchant_code\"";
  private static final String STORE_ID = "10001";
  private static final String CATEGORY_CODE = "category_code";
  private static final String QUERY_1 = "salesCatalog:CAT-CODE AND (productSku:\"PRODUCT-NAME\" OR productName:\"PRODUCT-NAME\")";
  private static final String QUERY_2 = "storeId:\"10001\"";
  private static final String QUERY_3 = "salesCatalog:CAT-CODE AND productSku:(\"PRODUCT-SKU-1\" OR \"PRODUCT-SKU-2\")";
  private static final String QUERY_4 = "masterCatalog:(category_code OR category_code) AND -salesCatalog:*";
  private static final String BRAND = "brand";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private QueryResponse queryResponse;
  private ProductSolr productSolr;
  private ProductSkuSummaryRequestVo productSkuSummaryRequestVo;
  private ProductSummaryRequestVo productSummaryRequestVo;
  private Item item;
  private ProductSummaryRequestV2Vo productSummaryRequestV2Vo;
  private ReelProductListingRequestVo reelProductListingRequestVo;
  private Product product;

  @Mock
  private CloudSolrClient cloudSolrClient;

  @Mock
  private QueryResponse groupQueryResponse;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private ProductSolrRepositoryImpl productSolrRepositoryImpl;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> solrQueryStatementArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<String>> solrQueryStatementArgumentCaptorList;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModel;

  private SolrDocument solrDocument1;

  private SolrDocumentList solrDocuments;

  @BeforeEach
  public void init() {
    openMocks(this);
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.MASTER_CATALOG, CATEGORY_CODE + "#_#" + CATEGORY_CODE);
    solrDocument.setField(SolrFieldNames.IS_ARCHIVED, Boolean.TRUE);
    solrDocument.setField(SolrFieldNames.IS_IN_STOCK, Boolean.TRUE);
    solrDocument.setField(SolrFieldNames.CURATION_STATUS, 1);
    solrDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, Boolean.TRUE);
    solrDocument.setField(SolrFieldNames.MINIMUM_LIST_PRICE, 100.0);
    solrDocument.setField(SolrFieldNames.MINIMUM_SELLING_PRICE,100.0);
    solrDocument.setField(SolrFieldNames.MAXIMUM_LIST_PRICE, 100.0);
    solrDocument.setField(SolrFieldNames.MAXIMUM_SELLING_PRICE, 100.0);
    solrDocument.setField(SolrFieldNames.PICKUP_POINT_CODES, Arrays.asList(PICKUP_POINT_CODE));
    solrDocument.setField(SolrFieldNames.BRAND, BRAND);

    solrDocument1 = new SolrDocument();
    BeanUtils.copyProperties(solrDocument, solrDocument1);
    solrDocument1.setField(SolrFieldNames.BRAND, "no ".concat(BRAND));
    solrDocuments = new SolrDocumentList();
    solrDocuments.add(solrDocument);
    NamedList<Object> response = new NamedList<>();
    response.add("response", solrDocuments);

    queryResponse = new QueryResponse();
    item = new Item();
    item.setProductSku(PRODUCT_SKU);
    item.setItemSku(PRODUCT_SKU);
    queryResponse.setResponse(response);
    productSolr = ProductSolr.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU).merchantCode(MERCHANT_CODE)
        .isSynchronized(false).masterCatalog(CATEGORY_CODE + "#_#" + CATEGORY_CODE).isArchived(true)
        .off2OnChannelActive(true).inStock(true).minimumListPrice(100.0).minimumSellingPrice(100.0)
        .maximumListPrice(100.0).maximumSellingPrice(100.0).pickupPointCodes(Arrays.asList(PICKUP_POINT_CODE))
        .curationStatus(1).brand(BRAND).build();
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "solrStringDelimiter", "#_#");
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "channelDefaultValue", "DEFAULT");
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSkuSummaryRequestVo = ProductSkuSummaryRequestVo.builder().brand(Arrays.asList(BRAND))
        .productSkus(Arrays.asList(PRODUCT_SKU)).categoryCodes(Arrays.asList(CATEGORY_CODE))
        .sortOrder("DESC").productSkuName(PRODUCT_NAME).build();

    productSummaryRequestV2Vo = new ProductSummaryRequestV2Vo();
    productSummaryRequestV2Vo.setMerchantCode(MERCHANT_CODE);
    productSummaryRequestV2Vo.setKeyword(PRODUCT_NAME);
    productSummaryRequestV2Vo.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    productSummaryRequestV2Vo.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    productSummaryRequestV2Vo.setBrands(Arrays.asList(BRAND));
    productSummaryRequestV2Vo.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    productSummaryRequestV2Vo.setMinPrice(100.0);
    productSummaryRequestV2Vo.setMaxPrice(100.0);
    productSummaryRequestV2Vo.setIsArchived(true);
    productSummaryRequestV2Vo.setOff2OnChannelActive(true);
    productSummaryRequestV2Vo.setFreeSample(true);
    productSummaryRequestV2Vo.setTradingProduct(true);
    productSummaryRequestV2Vo.setSortField("productSku");
    productSummaryRequestV2Vo.setSortOrder("asc");

    product = new Product();
    product.setProductSku(PRODUCT_SKU);

    reelProductListingRequestVo = new ReelProductListingRequestVo();
    reelProductListingRequestVo.setMerchantCode(MERCHANT_CODE);
    reelProductListingRequestVo.setKeyword(PRODUCT_SKU);
    reelProductListingRequestVo.setTradingProduct(Boolean.FALSE);
    reelProductListingRequestVo.setInStock(Boolean.FALSE);
    reelProductListingRequestVo.setCategoryCodes(List.of(CATEGORY_CODE));
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(cloudSolrClient);
    verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void findOneByProductSkuMarkForDeleteFalseTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ProductSolr result = productSolrRepositoryImpl.findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(productSolr, result);
  }

  @Test
  public void findOneByProductSkuMarkForDeleteFalse_expectException() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.findOneByProductSkuAndMarkForDeleteFalse(PRODUCT_SKU));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void findByProductSkuTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ProductSolr result = productSolrRepositoryImpl.findByProductSku(MERCHANT_CODE, PRODUCT_SKU);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(productSolr, result);
  }

  @Test
  public void findByProductSkuWithNullMerchantCodeTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ProductSolr result = productSolrRepositoryImpl.findByProductSku(null, PRODUCT_SKU);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(productSolr, result);
  }

  @Test
  public void findByProductSku_expectException() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.findByProductSku(null, PRODUCT_SKU));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void deleteSolrDocumentsByListOfProductSkuTest() throws Exception {
    when(cloudSolrClient.deleteById(Mockito.anyList())).thenReturn(new UpdateResponse());
    productSolrRepositoryImpl.deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU));
    verify(this.cloudSolrClient).deleteById(Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  public void deleteSolrDocumentsByListOfProductSkuEmptyTest() throws Exception {
    productSolrRepositoryImpl.deleteSolrDocumentsByListOfProductSku(new HashSet<>());
  }

  @Test
  public void deleteSolrDocumentsByListOfProductSkuTest_exception() throws Exception {
    when(cloudSolrClient.deleteById(Mockito.anyList())).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.deleteSolrDocumentsByListOfProductSku(Collections.singleton(PRODUCT_SKU)));
    } finally {
      verify(this.cloudSolrClient).deleteById(Collections.singletonList(PRODUCT_SKU));
    }
  }

  @Test
  public void findByProductCodeTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductSolr> result = productSolrRepositoryImpl.findByProductCode(PRODUCT_CODE, true);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(Collections.singletonList(productSolr), result);
  }

  @Test
  public void findByProductCodeTest_exception() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.findByProductCode(PRODUCT_CODE, null));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }


  @Test
  public void getProductCenterSummaryTest() throws Exception {
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getProductCenterSummary(STORE_ID, new ProductCenterSummaryRequest("CAT-CODE", "PRODUCT-NAME"),
            PageRequest.of(0, 100));
    assertEquals(QUERY_1, solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(Integer.valueOf(0), solrQueryArgumentCaptor.getValue().getStart());
    assertEquals(Integer.valueOf(100), solrQueryArgumentCaptor.getValue().getRows());
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.getValue());
  }

  @Test
  public void getProductCenterSummaryNoFilterTest() throws Exception {
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getProductCenterSummary(STORE_ID, new ProductCenterSummaryRequest(), PageRequest.of(0, 100));
    assertEquals(QUERY_2, solrQueryArgumentCaptor.getValue().getQuery());
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.getValue());
  }

  @Test
  public void getProductCenterSummaryCommasSeparatedTest() throws Exception {
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getProductCenterSummary(STORE_ID, new ProductCenterSummaryRequest("CAT-CODE", "PRODUCT-SKU-1,PRODUCT-SKU-2"),
            PageRequest.of(0, 100));
    assertEquals(QUERY_3, solrQueryArgumentCaptor.getValue().getQuery());
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.getValue());
  }

  @Test
  public void getProductCenterSummaryExceptionTest() throws Exception {
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenThrow(SolrException.class);
    try {
      Assertions.assertThrows(Exception.class, () ->  productSolrRepositoryImpl
          .getProductCenterSummary(STORE_ID, new ProductCenterSummaryRequest("CAT-CODE", "PRODUCT-NAME"),
              PageRequest.of(0, 100)));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.getValue());
    }
  }

  @Test
  public void getUnmappedProductSKusFromCategoryCodesTest() throws Exception {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "solrMaxRowSize", 1);
    queryResponse.getResults().setNumFound(2);
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(queryResponse);
    List<ProductSolr> response = productSolrRepositoryImpl
        .getUnmappedProductSKusByCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE, CATEGORY_CODE));
    verify(cloudSolrClient, times(2)).query(solrQueryArgumentCaptor.getValue());
    assertEquals(QUERY_4, solrQueryArgumentCaptor.getValue().getQuery());
  }

  @Test
  public void getUnmappedProductSKusFromCategoryCodesExceptionTest() throws Exception {
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenThrow(SolrException.class);
    try {
      Assertions.assertThrows(Exception.class, () ->  productSolrRepositoryImpl
          .getUnmappedProductSKusByCategoryCodes(STORE_ID, Arrays.asList(CATEGORY_CODE, CATEGORY_CODE)));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.getValue());
    }
  }

  @Test
  public void countByProductCodeAndMarkForDeleteFalseTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Long result = productSolrRepositoryImpl.countByProductCode(PRODUCT_CODE);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertNotNull(result);
  }

  @Test
  public void countByProductCodeAndMarkForDeleteFalseExceptionTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.countByProductCode(PRODUCT_CODE));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getProductSkuSummaryTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, MERCHANT_CODE, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
            .append(MERCHANT_CODE)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_NAME).append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_SKU)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(CATEGORY_CODE)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(BRAND)
            .append(SolrConstants.CLOSING_BRACKET)));
  }

  @Test
  public void getProductSkuSummaryBusinessPartnerAndProductNameNotNullNull() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    productSkuSummaryRequestVo.setProductSkuName(PRODUCT_NAME);
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, null, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getProductSkuSummaryBusinessPartnerAndProductSkuNotNull() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    productSkuSummaryRequestVo.setProductSkus(Arrays.asList(PRODUCT_SKU));
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, null, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getProductSkuSummaryBusinessPartnerAndCategoryCodeNotNull() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    productSkuSummaryRequestVo.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, null, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getProductSkuSummaryBusinessPartnerAndCategoryBrandNotNull() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    productSkuSummaryRequestVo.setBrand(Arrays.asList(BRAND));
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, null, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getProductSkuSummary_emptyRequestTest() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, MERCHANT_CODE, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
            .append(MERCHANT_CODE)));
  }

  @Test
  public void getProductSkuSummary_archivedTrueQueryTest() throws Exception {
    productSkuSummaryRequestVo.setIsArchived(Boolean.TRUE);
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, MERCHANT_CODE, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
            .append(MERCHANT_CODE)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_NAME)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_SKU)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(CATEGORY_CODE)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(BRAND)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries()).contains(
        new StringBuilder().append(SolrFieldNames.IS_ARCHIVED).append(SolrConstants.COLON)
            .append(true).toString()));
  }

  @Test
  public void getProductSkuSummary_archivedFalseQueryTest() throws Exception {
    productSkuSummaryRequestVo.setIsArchived(Boolean.FALSE);
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, MERCHANT_CODE, 0, 10);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MERCHANT_CODE).append(SolrConstants.COLON)
            .append(MERCHANT_CODE)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_NAME)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.PRODUCT_SKU).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(PRODUCT_SKU)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.MASTER_CATALOG).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(CATEGORY_CODE)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        new StringBuilder().append(SolrFieldNames.BRAND).append(SolrConstants.COLON)
            .append(SolrConstants.OPEN_BRACKET).append(BRAND)
            .append(SolrConstants.CLOSING_BRACKET)));
    Assertions.assertTrue(Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries()).contains(
        new StringBuilder().append(SolrFieldNames.IS_ARCHIVED).append(SolrConstants.COLON)
            .append(false).toString()));
  }

  @Test
  public void getProductSkuSummary_exceptionTest() throws Exception {
    productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    Mockito.when(this.cloudSolrClient.query(Mockito.any())).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl
          .getProductSkuSummary(STORE_ID, productSkuSummaryRequestVo, MERCHANT_CODE, 0, 10));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void updateStockStatusTest() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    productSolrRepositoryImpl.updateStockStatus(SolrFieldNames.PRODUCT_SKU, true, MERCHANT_CODE);
    Mockito.verify(cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateStockStatusViaEventTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "eventBasedSolrUpdateEnable", true);
    productSolrRepositoryImpl.updateStockStatus(PRODUCT_SKU, true, MERCHANT_CODE);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
      productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateStockStatusIOExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(IOException.class);
      productSolrRepositoryImpl.updateStockStatus(SolrFieldNames.PRODUCT_SKU, true, MERCHANT_CODE);
    } catch (Exception e) {
      Assertions.assertEquals(IOException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void updateStockStatusSolrServerExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
      productSolrRepositoryImpl.updateStockStatus(SolrFieldNames.PRODUCT_SKU, true, MERCHANT_CODE);
    } catch (Exception e) {
      Assertions.assertEquals(SolrServerException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void updateStockStatusSolrExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrException.class);
      productSolrRepositoryImpl.updateStockStatus(SolrFieldNames.PRODUCT_SKU, true, MERCHANT_CODE);
    } catch (Exception e) {
      Assertions.assertEquals(SolrException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.any(SolrInputDocument.class));
    }
  }

  @Test
  public void updatePromoOrWholesaleItemSkus() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    Mockito.verify(cloudSolrClient).add(Mockito.anyList());
  }

  @Test
  public void updatePromoOrWholesaleItemSkusViaEventTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "eventBasedSolrUpdateEnable", true);
    productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), eq(PRODUCT_SKU),
      productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
  }
  @Test()
  public void updatePromoOrWholesaleItemSkusViaEventClassCastTest() throws IOException,
    SolrServerException {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "eventBasedSolrUpdateEnable", true);
    productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(itemPickupPoint), true);
    Assertions.assertNotNull(reelProductListingRequestVo);
  }

  @Test
  public void updatePromoOrWholesaleItemSkusViaEventEmptyListTest() throws IOException,
    SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "eventBasedSolrUpdateEnable", true);
    productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Collections.emptyList(), true);
  }

  @Test
  public void updatePromoOrWholesaleItemSkusIOExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.anyList())).thenThrow(IOException.class);
      productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    } catch (Exception e) {
      Assertions.assertEquals(IOException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.anyList());
    }
  }

  @Test
  public void updatePromoOrWholesaleItemSkusSolrServerExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.anyList())).thenThrow(SolrServerException.class);
      productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    } catch (Exception e) {
      Assertions.assertEquals(SolrServerException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.anyList());
    }
  }

  @Test
  public void updatePromoOrWholesaleItemSkusSolrExceptionTest() throws SolrServerException, IOException {
    try {
      Mockito.when(cloudSolrClient.add(Mockito.anyList())).thenThrow(SolrException.class);
      productSolrRepositoryImpl.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    } catch (Exception e) {
      Assertions.assertEquals(SolrException.class, e.getClass());
    } finally {
      Mockito.verify(cloudSolrClient).add(Mockito.anyList());
    }
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequest() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(true).archived(true).merchantCode(MERCHANT_CODE)
        .sizeChartCode(SIZE_CHART_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestForNoNDistributionProductsForPureExternalRanchEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "distributionSellerList", new HashSet<>(Arrays.asList(MERCHANT_CODE)));
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(true).archived(true).merchantCode(MERCHANT_CODE)
        .sizeChartCode(SIZE_CHART_CODE).pureExternalUser(true).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    System.out.println("Filter queries: " + filterQueries);
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.DISTRIBUTION_STATUS + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
            + DistributionStatus.NON_DISTRIBUTION.getCode() + SolrConstants.SPACE + SolrConstants.OR_CLAUSE.trim()
            + SolrConstants.SPACE + DistributionStatus.DISTRIBUTION.getCode() + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestForNoNDistributionProductsForPureExternalRanchNotEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "ranchIntegrationEnabled", false);
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "distributionSellerList", new HashSet<>(Arrays.asList(MERCHANT_CODE)));
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(true).archived(true).merchantCode(MERCHANT_CODE)
        .sizeChartCode(SIZE_CHART_CODE).pureExternalUser(true).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    System.out.println("Filter queries: " + filterQueries);
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.DISTRIBUTION_STATUS + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
            + DistributionStatus.NON_DISTRIBUTION.getCode() + SolrConstants.SPACE + DistributionStatus.DISTRIBUTION.getCode()
            + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestForNoNDistributionProductsForPureExternalRanchEnabledAndNonRanchSeller() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "distributionSellerList", new HashSet<>(Arrays.asList(MERCHANT_CODE)));
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(true).archived(true).merchantCode(MERCHANT_CODE1)
        .sizeChartCode(SIZE_CHART_CODE).pureExternalUser(true).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    System.out.println("Filter queries: " + filterQueries);
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(MERCHANT_CODE1, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.DISTRIBUTION_STATUS + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
            + DistributionStatus.NON_DISTRIBUTION.getCode() + SolrConstants.SPACE + DistributionStatus.DISTRIBUTION.getCode()
            + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestForDistributionProductsForAmphiUserRanchEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "ranchIntegrationEnabled", true);
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "distributionSellerList", new HashSet<>(Arrays.asList(MERCHANT_CODE)));
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(true).archived(true).merchantCode(MERCHANT_CODE)
        .sizeChartCode(SIZE_CHART_CODE).pureExternalUser(false).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.DISTRIBUTION_STATUS + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
            + DistributionStatus.NON_DISTRIBUTION.getCode() + SolrConstants.SPACE + DistributionStatus.DISTRIBUTION.getCode()
            + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestB2bAndB2c() throws IOException, SolrServerException {
    productSummaryRequestVo =
        ProductSummaryRequestVo.builder().inStock(true).archived(true).b2cActivated(true).b2bActivated(true)
            .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 6);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.B2B_ACTIVATED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(
        SolrConstants.NOT_IN + SolrFieldNames.B2C_ACTIVATED + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestB2bAndB2cFalse() throws IOException, SolrServerException {
    productSummaryRequestVo =
        ProductSummaryRequestVo.builder().inStock(true).archived(true).b2cActivated(false).b2bActivated(false)
            .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 6);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.B2B_ACTIVATED + SolrConstants.COLON + false));
    assertTrue(filterQueries.contains(SolrFieldNames.B2C_ACTIVATED + SolrConstants.COLON + false));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestBooleanFlagsFalse() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(false).archived(false)
        .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSuspendedTrue() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(false).archived(false).suspended(true)
        .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSuspendedFalse() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().inStock(false).archived(false).suspended(false)
        .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 5);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestBooleanFlagsNull() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortOrderDesc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.DESC).build();
    productSummaryRequestVo.setPromoTypes(Collections.singletonList(SolrConstants.BUNDLE_PRODUCT_TYPE));
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortOrderAsc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldNPAndNoSortOrder() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(null).sortField(SolrConstants.NORMAL_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_LIST_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldNPAndSortOrderDesc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.DESC).sortField(SolrConstants.NORMAL_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_LIST_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldNPAndSortOrderAsc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC).sortField(SolrConstants.NORMAL_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_LIST_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldSPAndNoSortOrder() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(null).sortField(SolrConstants.SELLING_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldSPAndSortOrderDesc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.DESC).sortField(SolrConstants.SELLING_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldSPAndSortOrderAsc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC).sortField(SolrConstants.SELLING_PRICE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldUpdatedDateAndNoSortOrder() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(null).sortField(SolrConstants.UPDATED_DATE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.UPDATED_DATE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldUpdatedDateAndSortOrderDesc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.DESC).sortField(SolrConstants.UPDATED_DATE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.UPDATED_DATE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSortFieldUpdatedDateAndSortOrderAsc() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder()
        .merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC).sortField(SolrConstants.UPDATED_DATE_SORT).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.UPDATED_DATE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }


  @Test
  public void getL3ProductSummaryByProductSummaryRequestPickupPointCodesNotEmpty() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).pickupPointCodes(Arrays.asList(MERCHANT_CODE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestCategoryCodesNotEmpty() throws IOException, SolrServerException {
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).categoryCodes(Arrays.asList(MERCHANT_CODE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestPromoTypesPromoWithPromoFilterDisabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", false);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).promoTypes(Arrays.asList(SolrConstants.PROMO_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestPromoTypesPromoWithPromoFilterEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).promoTypes(Arrays.asList(SolrConstants.PROMO_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithInStoreFilterEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT)
        .promoTypes(Arrays.asList(SolrConstants.PROMO_PROMO_TYPE, SolrConstants.IN_STORE_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithPreOrderAndWholesaleFilterEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT)
        .promoTypes(Arrays.asList(SolrConstants.WHOLESALE_PROMO_TYPE, SolrConstants.PREORDER_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSummaryRequestVo.setBundleProduct(false);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithPreOrderAndWholesaleFilterDisabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", false);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT)
        .promoTypes(Arrays.asList(SolrConstants.WHOLESALE_PROMO_TYPE, SolrConstants.PREORDER_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithPreOrderAndWholesaleInvalidSorttypeAndPromoType() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", false);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(MERCHANT_CODE).promoTypes(Arrays.asList(MERCHANT_CODE, SolrConstants.PREORDER_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithPreOrderAndWholesaleInvalidSortTypeAndNullSortOrder() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", false);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(null)
        .sortField(MERCHANT_CODE).promoTypes(Arrays.asList(MERCHANT_CODE, SolrConstants.PREORDER_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithPreOrderAndWholesaleInvalidSortTypeAndDescSortOrder() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", false);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.DESC)
        .sortField(MERCHANT_CODE).promoTypes(Arrays.asList(MERCHANT_CODE, SolrConstants.PREORDER_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestWithAllFiltersEnabled() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).promoTypes(Arrays
            .asList(SolrConstants.WHOLESALE_PROMO_TYPE, SolrConstants.PREORDER_PROMO_TYPE,
                SolrConstants.IN_STORE_PROMO_TYPE, SolrConstants.PROMO_PROMO_TYPE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestPromoTypesPromoWithKeywordTest() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).promoTypes(Arrays
            .asList(SolrConstants.WHOLESALE_PROMO_TYPE, SolrConstants.PREORDER_PROMO_TYPE,
                SolrConstants.IN_STORE_PROMO_TYPE, SolrConstants.PROMO_PROMO_TYPE)).keyword(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF).equalsIgnoreCase("productName productSku"));
    assertEquals("100", solrQueryArgumentCaptor.getValue().get(SolrConstants.MM));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestAllFilters() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE)
        .inStock(true).archived(true).sortOrder(SolrConstants.ASC)
        .sortField(SolrConstants.SELLING_PRICE_SORT).promoTypes(Arrays
            .asList(SolrConstants.WHOLESALE_PROMO_TYPE, SolrConstants.PREORDER_PROMO_TYPE,
                SolrConstants.IN_STORE_PROMO_TYPE, SolrConstants.PROMO_PROMO_TYPE)).keyword(MERCHANT_CODE)
        .categoryCodes(Arrays.asList(MERCHANT_CODE)).pickupPointCodes(Arrays.asList(MERCHANT_CODE)).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 4);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrConstants.OPEN_BRACKET + SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON
            + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertTrue(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF).equalsIgnoreCase("productName productSku"));
    assertEquals("100", solrQueryArgumentCaptor.getValue().get(SolrConstants.MM));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.MINIMUM_SELLING_PRICE, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.asc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestNoFilters() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Assertions.assertEquals(1, response.getContent().size());
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSolrServerException() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductSolr> reponse = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Assertions.assertEquals(0, reponse.getContent().size());
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());

  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestSolrException() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductSolr> reponse = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Assertions.assertEquals(0, reponse.getContent().size());
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestIOException() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductSolr> reponse = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Assertions.assertEquals(0, reponse.getContent().size());
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestNoFiltersNullResponse() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    productSummaryRequestVo.setBundleProduct(true);
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(0, response.getContent().size());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getL3ProductSummaryByProductSummaryRequestNoFiltersEmptyResponse() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "promoFilterEnabled", true);
    productSummaryRequestVo = ProductSummaryRequestVo.builder().merchantCode(MERCHANT_CODE).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(new QueryResponse());
    Page<ProductSolr> response = productSolrRepositoryImpl
        .getL3ProductSummaryByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(0, response.getContent().size());
    Assertions.assertEquals(solrQueryArgumentCaptor.getValue().getFilterQueries().length, 2);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE_WITH_QUOTES));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PICKUP_POINT_CODES + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + MERCHANT_CODE
            + SolrConstants.CLOSING_BRACKET));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PROMO_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.WHOLESALE_ITEM_SKUS + SolrConstants.COLON + SolrConstants.SOLR_EXPRESSION_DELIMITER));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + MERCHANT_CODE + SolrConstants.CLOSING_BRACKET));
    assertNull(solrQueryArgumentCaptor.getValue().get(SolrConstants.QF));
    assertNotEquals(solrQueryArgumentCaptor.getValue().get(SolrConstants.MM), "100");
    assertFalse(filterQueries.contains(SolrFieldNames.IS_SUSPENDED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_IN_STOCK + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.contains(SolrFieldNames.IS_PRE_ORDER_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(SolrQuery.ORDER.desc, solrQueryArgumentCaptor.getValue().getSorts().get(0).getOrder());
  }

  @Test
  public void getActiveAndOosProductCountTest() throws IOException, SolrServerException {
    FacetField facetField = new FacetField(SolrFieldNames.IS_IN_STOCK);
    facetField.add("false", 5);
    facetField.add("true", 7);
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.setNumFound(12);
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(groupQueryResponse);
    when(groupQueryResponse.getFacetField(SolrFieldNames.IS_IN_STOCK)).thenReturn(facetField);
    when(groupQueryResponse.getResults()).thenReturn(solrDocuments);
    ProductCountResponseVo productCountResponseVo = productSolrRepositoryImpl.getActiveAndOosProductCount(MERCHANT_CODE);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    Assertions.assertEquals(7L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(5L, productCountResponseVo.getOutOfStock().longValue());
  }

  @Test
  public void getActiveAndOosProductCountNoInstockTest() throws IOException, SolrServerException {
    FacetField facetField = new FacetField(SolrFieldNames.IS_IN_STOCK);
    facetField.add("true", 7);
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.setNumFound(12);
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(groupQueryResponse);
    when(groupQueryResponse.getFacetField(SolrFieldNames.IS_IN_STOCK)).thenReturn(facetField);
    when(groupQueryResponse.getResults()).thenReturn(solrDocuments);
    ProductCountResponseVo productCountResponseVo = productSolrRepositoryImpl.getActiveAndOosProductCount(MERCHANT_CODE);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(12L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(0L, productCountResponseVo.getOutOfStock().longValue());
  }

  @Test
  public void getSuspendedAndArchivedProductCountTest() throws IOException, SolrServerException {
    FacetField facetField = new FacetField(SolrFieldNames.IS_SUSPENDED);
    facetField.add("false", 5);
    facetField.add("true", 7);
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.setNumFound(12);
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(groupQueryResponse);
    when(groupQueryResponse.getFacetField(SolrFieldNames.IS_SUSPENDED)).thenReturn(facetField);
    when(groupQueryResponse.getFacetField(SolrFieldNames.MARK_FOR_DELETE)).thenReturn(facetField);
    when(groupQueryResponse.getResults()).thenReturn(solrDocuments);
    ProductCountResponseVo
        productCountResponseVo = productSolrRepositoryImpl.getSuspendedAndArchivedProductCount(MERCHANT_CODE, new ProductCountResponseVo());
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(5L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(7L, productCountResponseVo.getSuspended().longValue());
  }

  @Test
  public void getSuspendedAndArchivedProductCountNoSuspendedTest() throws IOException, SolrServerException {
    FacetField facetField = new FacetField(SolrFieldNames.IS_SUSPENDED);
    facetField.add("false", 12);
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.setNumFound(12);
    when(cloudSolrClient.query(solrQueryArgumentCaptor.capture())).thenReturn(groupQueryResponse);
    when(groupQueryResponse.getFacetField(SolrFieldNames.IS_SUSPENDED)).thenReturn(facetField);
    when(groupQueryResponse.getFacetField(SolrFieldNames.MARK_FOR_DELETE)).thenReturn(facetField);
    when(groupQueryResponse.getResults()).thenReturn(solrDocuments);
    ProductCountResponseVo
        productCountResponseVo = productSolrRepositoryImpl.getSuspendedAndArchivedProductCount(MERCHANT_CODE, new ProductCountResponseVo());
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(MERCHANT_CODE, solrQueryArgumentCaptor.getValue().get(SolrConstants.ROUTE_KEY));
    Assertions.assertEquals(12L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(0L, productCountResponseVo.getSuspended().longValue());
  }

  @Test
  public void getProductNameByProductSummaryRequest() throws IOException, SolrServerException {
    productSummaryRequestVo =
        ProductSummaryRequestVo.builder().archived(false).merchantCode(MERCHANT_CODE).keyword(PRODUCT_NAME).build();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl
        .getProductNameByProductSummaryRequest(STORE_ID, productSummaryRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(SolrFieldNames.PRODUCT_SKU, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
  }

  @Test
  public void getHalalDashboardProductsResponseTest() throws SolrServerException, IOException {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo = new HalalDashboardFilterRequestVo();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoPage =
        productSolrRepositoryImpl.getHalalDashboardProductsResponse(STORE_ID, 0, 1, halalDashboardFilterRequestVo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertEquals(CATEGORY_CODE, halalDashboardProductsResponseVoPage.getContent().get(0).getCategoryCode());
    assertEquals(CurationStatus.NEED_CURATION.name(),
        halalDashboardProductsResponseVoPage.getContent().get(0).getCurationStatus());
    assertEquals(PRODUCT_CODE, halalDashboardProductsResponseVoPage.getContent().get(0).getProductCode());
  }

  @Test
  public void getHalalDashboardProductsResponseFilterTest() throws SolrServerException, IOException {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo = new HalalDashboardFilterRequestVo();
    halalDashboardFilterRequestVo.setCategories(Collections.singletonList(CATEGORY_CODE));
    halalDashboardFilterRequestVo.setBrands(Collections.singletonList(BRAND));
    halalDashboardFilterRequestVo.setCurationStatus(Arrays.asList(1, 2));
    halalDashboardFilterRequestVo.setKeyword(PRODUCT_NAME);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoPage =
        productSolrRepositoryImpl.getHalalDashboardProductsResponse(STORE_ID, 0, 1, halalDashboardFilterRequestVo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertEquals(CATEGORY_CODE, halalDashboardProductsResponseVoPage.getContent().get(0).getCategoryCode());
    assertEquals(CurationStatus.NEED_CURATION.name(),
        halalDashboardProductsResponseVoPage.getContent().get(0).getCurationStatus());
    assertEquals(PRODUCT_CODE, halalDashboardProductsResponseVoPage.getContent().get(0).getProductCode());
  }

  @Test
  public void getHalalDashboardProductsResponseNullQueryResponseTest() throws SolrServerException, IOException {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo = new HalalDashboardFilterRequestVo();
    halalDashboardFilterRequestVo.setCategories(Collections.singletonList(CATEGORY_CODE));
    halalDashboardFilterRequestVo.setBrands(Collections.singletonList(BRAND));
    halalDashboardFilterRequestVo.setCurationStatus(Arrays.asList(1, 2));
    halalDashboardFilterRequestVo.setKeyword(PRODUCT_NAME);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    Page<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoPage =
        productSolrRepositoryImpl.getHalalDashboardProductsResponse(STORE_ID, 0, 1, halalDashboardFilterRequestVo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertEquals(0, halalDashboardProductsResponseVoPage.getContent().size());
    assertEquals(0, halalDashboardProductsResponseVoPage.getTotalElements());
  }

  @Test
  public void getHalalDashboardProductsResponseEmptyQueryResponseTest() throws SolrServerException, IOException {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo = new HalalDashboardFilterRequestVo();
    halalDashboardFilterRequestVo.setCategories(Collections.singletonList(CATEGORY_CODE));
    halalDashboardFilterRequestVo.setBrands(Collections.singletonList(BRAND));
    halalDashboardFilterRequestVo.setCurationStatus(Arrays.asList(1, 2));
    halalDashboardFilterRequestVo.setKeyword(PRODUCT_NAME);
    QueryResponse queryResponse1 = new QueryResponse();
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse1);
    Page<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoPage =
        productSolrRepositoryImpl.getHalalDashboardProductsResponse(STORE_ID, 0, 1, halalDashboardFilterRequestVo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertEquals(0, halalDashboardProductsResponseVoPage.getContent().size());
    assertEquals(0, halalDashboardProductsResponseVoPage.getTotalElements());
  }

  @Test
  public void getHalalDashboardProductsResponseExceptionTest() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    productSolrRepositoryImpl.getHalalDashboardProductsResponse(STORE_ID, 0, 1, new HalalDashboardFilterRequestVo());
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }


  @Test
  public void getProductSummaryV2Test() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, new ProductSummaryRequestV2Vo());
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2TestForInStockTrue() throws SolrServerException, IOException {
    ProductSummaryRequestV2Vo productSummaryRequest = new ProductSummaryRequestV2Vo();
    productSummaryRequest.setMerchantCode(MERCHANT_CODE);
    productSummaryRequest.setInStock(Boolean.TRUE);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
      productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequest);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2ValuesTest() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2ValuesBrandFilterTest() throws SolrServerException, IOException {
    solrDocuments.add(solrDocument1);
    NamedList<Object> response = new NamedList<>();
    response.add("response", solrDocuments);
    queryResponse.setResponse(response);
    productSkuSummaryRequestVo.setBrand(Arrays.asList(BRAND, "no ".concat(BRAND),
      "yes ".concat(BRAND)));
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
      productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
    assertEquals(BRAND, productSummaryResponseV2VoPage.getContent().get(0).getBrand());
    assertNotEquals("yes ".concat(BRAND),
      productSummaryResponseV2VoPage.getContent().get(1).getBrand());
    assertEquals("no ".concat(BRAND),
      productSummaryResponseV2VoPage.getContent().get(1).getBrand());

  }

  @Test
  public void getProductSummaryV2ValuesDescOrderTest() throws SolrServerException, IOException {
    productSummaryRequestV2Vo.setSortOrder("desc");
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2ValuesNoOrderTest() throws SolrServerException, IOException {
    productSummaryRequestV2Vo.setSortOrder(null);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2NoValuesDescOrderTest() throws SolrServerException, IOException {
    productSummaryRequestV2Vo.setSortField(null);
    productSummaryRequestV2Vo.setSortOrder("desc");
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2NoValuesNoOrderTest() throws SolrServerException, IOException {
    productSummaryRequestV2Vo.setSortField(null);
    productSummaryRequestV2Vo.setSortOrder("asc");
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
        productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }

  @Test
  public void getProductSummaryV2NoValuesNoOrderExceptionTest() throws SolrServerException, IOException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "priceRangeSolrQuery", true);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, productSummaryRequestV2Vo);
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }


  @Test
  public void executeSolrDocumentsAtomicUpdateTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, PRODUCT_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    this.productSolrRepositoryImpl.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, PRODUCT_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
    this.productSolrRepositoryImpl.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateIOExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, PRODUCT_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(IOException.class);
    this.productSolrRepositoryImpl.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateRouteExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, PRODUCT_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    Mockito.when(cloudSolrClient.add(Mockito.any(SolrInputDocument.class)))
        .thenThrow(CloudSolrClient.RouteException.class);
    this.productSolrRepositoryImpl.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
  public void findByProductSkuListTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductSolr> result = productSolrRepositoryImpl.findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
        + SolrConstants.DOUBLE_QUOTE + PRODUCT_SKU + SolrConstants.DOUBLE_QUOTE
        + SolrConstants.CLOSING_BRACKET, solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(Arrays.asList(productSolr), result);
  }

  @Test
  public void findByProductSkuListEmptyProductSkuTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductSolr> result = productSolrRepositoryImpl.findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet);
    assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void findByProductSkuListTestExceptionCase() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->  productSolrRepositoryImpl.findByProductSkuList(STORE_ID, MERCHANT_CODE, productSkuSet));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getProductSummaryV2PriceFilterTest() throws SolrServerException, IOException {
    ReflectionTestUtils.setField(productSolrRepositoryImpl, "priceRangeSolrQuery", true);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSummaryResponseV2Vo> productSummaryResponseV2VoPage =
      productSolrRepositoryImpl.getProductSummaryV2(STORE_ID, 0, 1, new ProductSummaryRequestV2Vo());
    verify(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isArchived());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isInStock());
    assertTrue(productSummaryResponseV2VoPage.getContent().get(0).isOff2OnActiveFlag());
    assertEquals(CATEGORY_CODE, productSummaryResponseV2VoPage.getContent().get(0).getCategoryCode());
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxNormalPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMaxSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinSellingPrice(), 0);
    assertEquals(100.0, productSummaryResponseV2VoPage.getContent().get(0).getMinNormalPrice(), 0);
  }
  @Test
  public void testGetActiveProductsByStoreIdAndSizeChartCode_success_empty_listTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 10;
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.setNumFound(1);
    QueryResponse queryResponse = Mockito.mock(QueryResponse.class);
    Mockito.when(queryResponse.getResults()).thenReturn(solrDocumentList);
    Mockito.when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSkuSizeChartResponse> result = productSolrRepositoryImpl.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    Mockito.verify(cloudSolrClient).query(any(SolrQuery.class));
    assertNotNull(result);
    assertEquals(0, result.getTotalElements());
    assertEquals(page, result.getNumber());
    assertEquals(size, result.getSize());
  }
  @Test
  public void testGetActiveProductsByStoreIdAndSizeChartCode_successTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 10;
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocumentList.setNumFound(1);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, "PRODUCT_SKU_VALUE");
    solrDocument.setField(SolrFieldNames.SIZE_CHART_CODE, sizeChartCode);
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = Mockito.mock(QueryResponse.class);
    Mockito.when(queryResponse.getResults()).thenReturn(solrDocumentList);
    Mockito.when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductSkuSizeChartResponse> result =
        productSolrRepositoryImpl.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    Mockito.verify(cloudSolrClient).query(any(SolrQuery.class));
    assertEquals("PRODUCT_SKU_VALUE", result.getContent().get(0).getProductSku());  // Verify the product SKU value
  }

  @Test
  public void testGetActiveProductsByStoreIdAndSizeChartCode_nullTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 10;
    QueryResponse queryResponse = Mockito.mock(QueryResponse.class);
    Mockito.when(queryResponse.getResults()).thenReturn(null);
    Mockito.when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(null);
    Page<ProductSkuSizeChartResponse> result =
        productSolrRepositoryImpl.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    Mockito.verify(cloudSolrClient).query(any(SolrQuery.class));
  }

  @Test
  public void testGetActiveProductsByStoreIdAndSizeChartCode_exceptionTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 10;
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Page<ProductSkuSizeChartResponse> result =
          productSolrRepositoryImpl.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void findByProductSkuListForMFDTrueAndFalseEmptyProductSkuTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductSolr> result = productSolrRepositoryImpl.findByProductSkuListForMFDTrueAndFalse(STORE_ID, MERCHANT_CODE, productSkuSet);
    assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void findByProductSkuListForMFDTrueAndFalseTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductSolr> result = productSolrRepositoryImpl.findByProductSkuListForMFDTrueAndFalse(STORE_ID, MERCHANT_CODE, productSkuSet);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.OPEN_BRACKET
        + SolrConstants.DOUBLE_QUOTE + PRODUCT_SKU + SolrConstants.DOUBLE_QUOTE
        + SolrConstants.CLOSING_BRACKET, solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(Arrays.asList(productSolr), result);
  }

  @Test
  public void getL3ProductsForReelsByReelProductListingRequestTest()
      throws SolrServerException, IOException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl.getL3ProductsForReelsByReelProductListingRequest(STORE_ID,
        reelProductListingRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(112, solrQueryArgumentCaptor.getValue().getFields().length());
  }

  @Test
  public void getL3ProductsForReelsByReelProductListingRequestWithEmptyResponseTest()
      throws SolrServerException, IOException {
    queryResponse = new QueryResponse();
    queryResponse.setResponse(new NamedList<>());
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl.getL3ProductsForReelsByReelProductListingRequest(STORE_ID,
        reelProductListingRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(112, solrQueryArgumentCaptor.getValue().getFields().length());
  }

  @Test
  public void getL3ProductsForReelsByEmptyReelProductListingRequestTest()
      throws SolrServerException, IOException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    productSolrRepositoryImpl.getL3ProductsForReelsByReelProductListingRequest(STORE_ID,
        new ReelProductListingRequestVo(), PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(112, solrQueryArgumentCaptor.getValue().getFields().length());
  }

  @Test
  public void getL3ProductsForReelsByReelProductListingRequestExceptionTest()
      throws SolrServerException, IOException {
    Mockito.doThrow(new IOException()).when(cloudSolrClient).query(Mockito.any(SolrQuery.class));
    productSolrRepositoryImpl.getL3ProductsForReelsByReelProductListingRequest(STORE_ID,
        new ReelProductListingRequestVo(), PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(112, solrQueryArgumentCaptor.getValue().getFields().length());
  }

  @Test
  void getL3ProductsForReelsByReelProductListingRequestWithProductSkusListTest()
      throws SolrServerException, IOException {
    List<String> productSkus = List.of(PRODUCT_SKU);
    reelProductListingRequestVo.setProductSkuList(productSkus);
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productSolrRepositoryImpl.getL3ProductsForReelsByReelProductListingRequest(STORE_ID,
        reelProductListingRequestVo, PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(112, solrQueryArgumentCaptor.getValue().getFields().length());
  }


}
