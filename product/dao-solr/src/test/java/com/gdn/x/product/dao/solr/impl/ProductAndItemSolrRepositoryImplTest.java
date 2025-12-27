package com.gdn.x.product.dao.solr.impl;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrRequest;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.BaseCloudSolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.Group;
import org.apache.solr.client.solrj.response.GroupCommand;
import org.apache.solr.client.solrj.response.GroupResponse;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.util.NamedList;
import org.joda.time.DateTime;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.SolrGroupResultVO;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class ProductAndItemSolrRepositoryImplTest {

  @Mock
  private CloudSolrClient cloudSolrClient;

  @Mock
  private CloudSolrClient cloudSolrClientL3;

  @Mock
  private QueryResponse groupQueryResponse;

  @Mock
  private GroupResponse groupResponse;

  @Mock
  private GroupCommand groupCommand;

  @Mock
  private Group group;

  @Mock
  private QueryResponse solrQueryResponse;

  @Mock
  private SystemParameterService systemParameterService;

  @InjectMocks
  private ProductAndItemSolrRepositoryImpl productAndItemSolrRepository;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryArgumentCaptor;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModel;


  private static final String FIRST_ELEMENT = "first";
  private static final String SECOND_ELEMENT = "second";
  private static final String ID = "id";
  private static final String STORE_ID = "10001";
  private static final String ITEM_CODE = "itemCode";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SALES_CATEGORY_CODE = "salesCategoryCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String DELIMITER = "delimiter";
  private static final double OFFER_PRICE = 1000D;
  private static final String MASTER_CATEGORY_CODE = "masterCategoryCode";
  private static final String PICKUPPOINT_CODE = "pickuppointCode";
  private static final String ITEM_SKU = SolrFieldNames.ITEM_SKU;
  private static final String ITEM_NAME = "itemName";
  private static final String PRODUCT_NAME = "productName";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String PRODUCT_SKU = SolrFieldNames.PRODUCT_SKU;
  private static final String BRAND = "dummy-brand";
  private static final String SORT_BY = "asc";
  private static final String SORT_BY_DESC = "desc";
  private static final String ORDER_BY = "createdDate";
  private static final String CHANNEL_NAME = "channelName";
  private static final String PICKUP_POINT = "pickupPoint";
  private static final String PICKUP_POINT_1 = "pickupPoint1";
  private static final String PICKUP_POINT_2 = "pickupPoint2";
  private static final String PRODUCT_CATENTRY_ID = "PRODUCT_CATENTRY_ID";
  private static final String TICKET_TEMPLATE_CODE = "TICKET_TEMPLATE_CODE";
  private static final String KEY = "key";
  private static final String ACTIVE = "Active";
  private static final String SUSPENDED = "Suspended";
  private static final String SEARCH_KEY = "searchKey";
  private static final String SOLR_DELIMITER = "#_#";
  private static final String BRAND_QUOTES = "\"dummy-brand\"";
  private static final String PRISTINE_ID = "pristineId";
  private static final String ALL = "All";
  private OfficialStoreRequestVO officialStoreRequestVO;
  private ActiveProductsRequestVO activeProductsRequestVO;
  private static final String CHANNEL = ChannelName.ANDROID.toString();
  private static final PageRequest PAGE_REQUEST = PageRequest.of(0, 10);
  private static final List<String> productSkusList = new ArrayList<>(Arrays.asList("SKU-123","SKU-234","SKU-345"));
  private static final List<String> itemSkusList = new ArrayList<>(Arrays.asList("SKU-123-001","SKU-234-002","SKU-345-001"));


  private List<ProductAndItemSolr> productAndItemsSolr;
  private SolrDocument solrDocument;
  private SolrDocumentList solrDocumentList;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrDocumentListArgumentCaptor;

  private QueryResponse queryResponse;
  private QueryResponse nullQueryResponse;
  private NamedList<Object> facetCount;

  private ProductAndItemSolr productAndItemSolr;
  private BulkItemSummaryRequestVo bulkItemSummaryRequestVo;
  private List<String> stringList;
  private List<String> pickupCodes;
  private ItemSummaryRequestVO itemSummaryRequestVO;
  private String channelName = "DEFAULT";
  private Set<String> setList;
  private static Pageable pageable = PageRequest.of(0, 10);
  private static long numFound = 10;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.ID, ID);
    solrDocument.setField(SolrFieldNames.PRODUCT_SKU, PRODUCT_SKU);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.ITEM_SKU, ITEM_SKU);
    solrDocument.setField(SolrFieldNames.ITEM_CODE, ITEM_CODE);
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.add(solrDocument);
    NamedList<Object> response = new NamedList<>();
    nullQueryResponse = new QueryResponse();
    response.add("response", solrDocuments);
    NamedList namedList = new NamedList();
    namedList.add("l5Count", 500.0);
    response.add("facets", namedList);
    queryResponse = new QueryResponse();
    queryResponse.setResponse(response);
    NamedList<Object> facetValue = new NamedList<>();
    facetValue.add(PRODUCT_SKU, 1);
    NamedList<Object> facets = new NamedList<>();
    facets.add("facet_fields", facetValue);
    NamedList<Object> facetFields = new NamedList<>();
    facetFields.add("facet_fields", facets);
    facetCount = new NamedList<>();
    facetCount.add("facet_counts", facetFields);
    ReflectionTestUtils.setField(productAndItemSolrRepository, "solrStringDelimiter", "#_#");
    ReflectionTestUtils.setField(productAndItemSolrRepository, "channelDefaultValue", "DEFAULT");
    ReflectionTestUtils.setField(productAndItemSolrRepository, "solrMaxRowSize", Integer.valueOf(10));
    productAndItemSolr = new ProductAndItemSolr();
    stringList = new ArrayList<>();
    stringList.add(FIRST_ELEMENT);
    stringList.add(SECOND_ELEMENT);
    pickupCodes = new ArrayList<>();
    pickupCodes.add(PICKUP_POINT);
    pickupCodes.add(PICKUP_POINT_1);
    pickupCodes.add(PICKUP_POINT_2);
    productAndItemSolr.setId(ID);
    itemSummaryRequestVO = new ItemSummaryRequestVO();
    itemSummaryRequestVO.setArchived(Boolean.TRUE);
    itemSummaryRequestVO.setBuyable(Boolean.FALSE);
    itemSummaryRequestVO.setCategoryCodes(stringList);
    itemSummaryRequestVO.setChannelName(channelName);
    itemSummaryRequestVO.setCncActivated(true);
    itemSummaryRequestVO.setDiscoverable(Boolean.FALSE);
    itemSummaryRequestVO.setIsTradingProduct(Boolean.FALSE);
    itemSummaryRequestVO.setItemCode(SolrFieldNames.ITEM_CODE);
    itemSummaryRequestVO.setItemSkuKeyword(SolrFieldNames.ITEM_SKU);
    itemSummaryRequestVO.setItemSkus(stringList);
    itemSummaryRequestVO.setMasterCategoryCode(SolrFieldNames.MASTER_CATALOG);
    itemSummaryRequestVO.setMerchantCode(SolrFieldNames.MERCHANT_CODE);
    itemSummaryRequestVO.setMerchantSkus(stringList);
    itemSummaryRequestVO.setOfferPrice(OFFER_PRICE);
    itemSummaryRequestVO.setPickupPointCode(SolrFieldNames.PICKUP_POINT_CODE);
    itemSummaryRequestVO.setProductCode(SolrFieldNames.PRODUCT_CODE);
    itemSummaryRequestVO.setProductItemName(SolrFieldNames.ITEM_NAME);
    itemSummaryRequestVO.setProductSkus(stringList);
    itemSummaryRequestVO.setPickupPointCodes(pickupCodes);
    itemSummaryRequestVO.setSearchKey(SEARCH_KEY);
    itemSummaryRequestVO.setSalesCategoryCode(SolrFieldNames.SALES_CATALOG);
    productAndItemSolr.setProductSku(SolrFieldNames.PRODUCT_SKU);
    productAndItemSolr.setProductCode(SolrFieldNames.PRODUCT_CODE);
    productAndItemSolr.setItemCode(SolrFieldNames.ITEM_CODE);
    productAndItemSolr.setItemSku(SolrFieldNames.ITEM_SKU);
    productAndItemSolr.setSynchronized(Boolean.FALSE);
    productAndItemsSolr = Collections.singletonList(productAndItemSolr);
    officialStoreRequestVO = new OfficialStoreRequestVO();
    officialStoreRequestVO.setBrands(stringList);
    officialStoreRequestVO.setCategoryCode(SolrFieldNames.MASTER_CATALOG);
    officialStoreRequestVO.setMaxPrice(OFFER_PRICE);
    officialStoreRequestVO.setMerchantCodes(stringList);
    officialStoreRequestVO.setMinPrice(OFFER_PRICE);
    officialStoreRequestVO.setProductName(SolrFieldNames.PRODUCT_NAME);
    officialStoreRequestVO.setProductSku(SolrFieldNames.PRODUCT_SKU);
    officialStoreRequestVO.setArchived(Boolean.FALSE);
    officialStoreRequestVO.setBuyable(Boolean.FALSE);
    officialStoreRequestVO.setDiscoverable(Boolean.FALSE);
    officialStoreRequestVO.setChannelName(CHANNEL_NAME);
    activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setCategoryCodes(stringList);
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setBuyable(Boolean.FALSE);
    activeProductsRequestVO.setDiscoverable(Boolean.FALSE);
    activeProductsRequestVO.setSearchKey(KEY);
    activeProductsRequestVO.setStatus("All");
    bulkItemSummaryRequestVo = new BulkItemSummaryRequestVo();
    bulkItemSummaryRequestVo.setMerchantCode(MERCHANT_CODE);
    bulkItemSummaryRequestVo.setArchived(Boolean.FALSE);
    bulkItemSummaryRequestVo.setBuyable(Boolean.FALSE);
    bulkItemSummaryRequestVo.setCncActivated(true);
    bulkItemSummaryRequestVo.setChannelName(CHANNEL_NAME);
    bulkItemSummaryRequestVo.setDiscoverable(Boolean.FALSE);
    bulkItemSummaryRequestVo.setOfferPrice(1.0);
    bulkItemSummaryRequestVo.setMasterCategoryCode(CATEGORY_CODE);
    bulkItemSummaryRequestVo.setItemSkuKeyword(ITEM_SKU);
    bulkItemSummaryRequestVo.setItemSkus(stringList);
    bulkItemSummaryRequestVo.setProductItemName(ITEM_NAME);
    bulkItemSummaryRequestVo.setProductCode(PRODUCT_CODE);
    bulkItemSummaryRequestVo.setItemCodes(stringList);
    bulkItemSummaryRequestVo.setSalesCategoryCode(CATEGORY_CODE);
    bulkItemSummaryRequestVo.setMerchantSku(MERCHANT_SKU);
    bulkItemSummaryRequestVo.setMerchantSkus(stringList);
    bulkItemSummaryRequestVo.setIsTradingProduct(Boolean.FALSE);
    bulkItemSummaryRequestVo.setPickupPointCode(PICKUP_POINT);
    setList = new HashSet();
    setList.add(ITEM_SKU);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(cloudSolrClient, groupQueryResponse, groupCommand, group, systemParameterService,
        cloudSolrClientL3, kafkaProducer);
  }

  @Test
  public void getListOfActiveProductSkusSortedByProductCode() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getListOfActiveProductSkusSortedByProductCode(STORE_ID, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getListOfActiveProductSkusSortedByProductCodeNullResultTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(nullQueryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getListOfActiveProductSkusSortedByProductCode(STORE_ID, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
  }

  @Test
  public void getListOfActiveProductSkusSortedByProductCodeSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getListOfActiveProductSkusSortedByProductCode(STORE_ID, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getListOfActiveProductSkusSortedByProductCodeSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getListOfActiveProductSkusSortedByProductCode(STORE_ID, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getListOfActiveProductSkusSortedByProductCodeExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getListOfActiveProductSkusSortedByProductCode(STORE_ID, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductAndItemsByFilter() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilterProductSkus() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    ItemSummaryRequestVO itemSummaryRequestVO1 = new ItemSummaryRequestVO();
    itemSummaryRequestVO1.setProductSkus(productSkusList);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO1, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.PRODUCT_SKU));
  }

  @Test
  public void getProductAndItemsByFilterItemSkus() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    ItemSummaryRequestVO itemSummaryRequestVO1 = new ItemSummaryRequestVO();
    itemSummaryRequestVO1.setItemSkus(itemSkusList);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO1, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.ITEM_SKU));
  }

  @Test
  public void getProductAndItemsByFilterProductAndItemSkus() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    ItemSummaryRequestVO itemSummaryRequestVO1 = new ItemSummaryRequestVO();
    itemSummaryRequestVO1.setProductSkus(productSkusList);
    itemSummaryRequestVO1.setItemSkus(itemSkusList);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO1, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(SolrFieldNames.ITEM_SKU));
  }

  @Test
  public void getProductAndItemsByFilterWithMarkForDeleteTrue() throws IOException, SolrServerException {
    itemSummaryRequestVO.setMarkForDelete(true);
    itemSummaryRequestVO.setOfferPrice(null);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertFalse(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilter_withExcludedItemSKUs() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    itemSummaryRequestVO.setExcludedItemSkus(Arrays.asList(ITEM_SKU, "item-sku-code"));

    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
      .getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));

    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -itemSku:(\"itemSku\",\"item-sku-code\")"));

    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilter_withLinkedPartnerCode() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    itemSummaryRequestVO.setLinkedPartnerCode("linked-partner-code");

    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
      .getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));

    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -linkedPartners:(\"linked-partner-code\")"));

    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilter_withCncActivatedFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    itemSummaryRequestVO.setCncActivated(Boolean.FALSE);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilter_withInvalidExcludedItemSKUs() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    itemSummaryRequestVO.setExcludedItemSkus(Arrays.asList("", null));

    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
      .getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));

    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -itemSku"));

    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilterNullOrderByTest() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, null, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilterEmptyCategoryCode() throws IOException, SolrServerException {
    itemSummaryRequestVO.setCategoryCodes(new ArrayList<>());
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY , PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
  }

  @Test
  public void getProductAndItemsByFilterSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY ,
              PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenBoostProductSkus() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.stream().anyMatch(filterQuery -> filterQuery.contains(SolrFieldNames.BUYABLE)));
    assertTrue(filterQueries.stream().anyMatch(filterQuery -> filterQuery.contains(SolrFieldNames.DISCOVERABLE)));
    assertFalse(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenBoostProductSkusEmpty() throws IOException, SolrServerException {
    itemSummaryRequestVO.setBoostProductSkus(Collections.emptyList());
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenNullOrderByTest() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, null, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY ,
              PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenOff2OnChannelFalse() throws IOException, SolrServerException {
    itemSummaryRequestVO.setOff2OnChannelActive(false);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getPromoProductAndItemsByFilter_whenOff2OnChannelTrue() throws IOException, SolrServerException {
    itemSummaryRequestVO.setOff2OnChannelActive(true);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    assertFalse(filterQueries.stream().anyMatch(filterQuery -> filterQuery.contains(SolrFieldNames.BUYABLE)));
    assertFalse(filterQueries.stream().anyMatch(filterQuery -> filterQuery.contains(SolrFieldNames.DISCOVERABLE)));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductAndItemsByFilterSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY,
              PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductAndItemsByFilterExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductAndItemsByFilter(STORE_ID, itemSummaryRequestVO, ORDER_BY, SORT_BY ,
              PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
      Assertions.assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrConstants.OPEN_BRACKET + SEARCH_KEY + SolrConstants.CLOSING_BRACKET));
    }
  }

  @Test
  public void getBulkProductAndItemsByFilter() throws Exception {
    bulkItemSummaryRequestVo.setPristineIds(stringList);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0,
          10), null, null);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -linkedPartners:(\"linked-partner-code\")"));
  }

  @Test
  public void getBulkProductAndItemsByFilter_withEmptyItemCode() throws Exception {
    bulkItemSummaryRequestVo.setPristineIds(stringList);
    bulkItemSummaryRequestVo.setItemCodes(new ArrayList<>());
    bulkItemSummaryRequestVo.setOfferPrice(null);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10), null, null);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -linkedPartners:(\"linked-partner-code\")"));
  }

  @Test
  public void getBulkProductAndItemsByFilter_withExcludedItemSKUs() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    bulkItemSummaryRequestVo.setExcludedItemSkus(Arrays.asList(ITEM_SKU, "item-sku-code"));
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
      .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10), null,
        null);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -itemSku:(\"itemSku\",\"item-sku-code\")"));

    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsByFilter_withLinkedPartnerCode() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    bulkItemSummaryRequestVo.setLinkedPartnerCode("linked-partner-code");
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
      .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10), null, null);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -linkedPartners:(\"linked-partner-code\")"));

    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsByFilter_withInvalidExcludedItemSKUs() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    bulkItemSummaryRequestVo.setExcludedItemSkus(Arrays.asList(null, "", "   "));
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
      .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10), null, null);

    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());

    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertFalse(solrQueryArgumentCaptor.getValue().getQuery().contains(" AND -itemSku:"));

    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsBySortFilter() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0,
          10), SORT_BY, ORDER_BY);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
        SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE));
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(ORDER_BY, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsBySortFilterDescSort() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10), SORT_BY_DESC,
            ORDER_BY);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE));
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(ORDER_BY, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsBySortFilterEmptyMerchantSku() throws Exception {
    bulkItemSummaryRequestVo.setMerchantSkus(new ArrayList<>());
    bulkItemSummaryRequestVo.setCncActivated(false);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0,
          10), SORT_BY, ORDER_BY);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE));
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(ORDER_BY, solrQueryArgumentCaptor.getValue().getSorts().get(0).getItem());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getBulkProductAndItemsBySortFilterExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10)
      , SORT_BY,
              ORDER_BY);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void getBulkProductAndItemsBySortFilterIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0,
              10), SORT_BY,
              ORDER_BY);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery().contains(
          SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE));
    }
  }

  @Test
  public void getBulkProductAndItemsBySortFilterSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .getBulkProductAndItemsByFilter(STORE_ID, bulkItemSummaryRequestVo, PageRequest.of(0, 10)
      , SORT_BY,
              ORDER_BY);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE));
    }
  }

  @Test
  public void getProductsForOfficialStore() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(groupQueryResponse);
    Mockito.when(this.groupQueryResponse.getGroupResponse()).thenReturn(groupResponse);
    Mockito.when(this.groupResponse.getValues()).thenReturn(Arrays.asList(groupCommand));
    Mockito.when(this.groupCommand.getValues()).thenReturn(Arrays.asList(group));
    Mockito.when(this.groupCommand.getName()).thenReturn(SolrFieldNames.PRODUCT_SKU);
    Mockito.when(this.groupCommand.getNGroups()).thenReturn(10);
    Mockito.when(this.group.getResult()).thenReturn(solrDocumentList);
    Mockito.when(this.group.getGroupValue()).thenReturn(SolrFieldNames.PRODUCT_SKU);
    SolrGroupResultVO<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    Mockito.verify(this.groupQueryResponse, times(3)).getGroupResponse();
    Mockito.verify(this.groupResponse, times(2)).getValues();
    Mockito.verify(this.groupCommand).getValues();
    Mockito.verify(this.groupCommand).getName();
    Mockito.verify(this.groupCommand).getNGroups();
    Mockito.verify(this.group).getResult();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertEquals(10L, result.getNumGroups());
  }

  @Test
  public void getProductsForOfficialStoreOff2OnChannelTrue() throws IOException, SolrServerException {
    officialStoreRequestVO.setOff2OnChannelActive(true);
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(groupQueryResponse);
    Mockito.when(this.groupQueryResponse.getGroupResponse()).thenReturn(groupResponse);
    Mockito.when(this.groupResponse.getValues()).thenReturn(Arrays.asList(groupCommand));
    Mockito.when(this.groupCommand.getValues()).thenReturn(Arrays.asList(group));
    Mockito.when(this.groupCommand.getName()).thenReturn(SolrFieldNames.PRODUCT_SKU);
    Mockito.when(this.groupCommand.getNGroups()).thenReturn(10);
    Mockito.when(this.group.getResult()).thenReturn(solrDocumentList);
    Mockito.when(this.group.getGroupValue()).thenReturn(SolrFieldNames.PRODUCT_SKU);
    SolrGroupResultVO<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE + SolrConstants.COLON + Boolean.TRUE));
    Mockito.verify(this.groupQueryResponse, times(3)).getGroupResponse();
    Mockito.verify(this.groupResponse, times(2)).getValues();
    Mockito.verify(this.groupCommand).getValues();
    Mockito.verify(this.groupCommand).getName();
    Mockito.verify(this.groupCommand).getNGroups();
    Mockito.verify(this.group).getResult();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
    Assertions.assertEquals(10L, result.getNumGroups());
  }

  @Test
  public void getActiveProductsListForMerchantTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    solrDocumentList.setNumFound(10);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    activeProductsRequestVO.setBundleProduct(false);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse, times(3)).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10L, result.getNumGroups());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10, result.getNumGroups());
    Assertions.assertEquals("-bundleProduct:true", solrQueryArgumentCaptor.getValue().getFilterQueries()[1]);
  }

  @Test
  public void getActiveProductsListForMerchantTradingProductTrueTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    solrDocumentList.setNumFound(10);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    activeProductsRequestVO.setTradingProduct(true);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse, times(3)).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10L, result.getNumGroups());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10, result.getNumGroups());
  }

  @Test
  public void getActiveProductsListForMerchantEmptyMerchantCodeTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    solrDocumentList.setNumFound(10);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    activeProductsRequestVO.setMerchantCode(null);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse, times(3)).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10L, result.getNumGroups());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10, result.getNumGroups());
  }

  @Test
  public void getActiveProductsListForMerchantCodeEmptyTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    solrDocumentList.setNumFound(10);
    activeProductsRequestVO.setMerchantCode(StringUtils.EMPTY);
    activeProductsRequestVO.setTradingProduct(true);
    activeProductsRequestVO.setBundleProduct(true);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse, times(3)).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10L, result.getNumGroups());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10, result.getNumGroups());
  }

  @Test
  public void getActiveProductsListForMerchantTestCNCTrue()
      throws IOException, SolrServerException, InvocationTargetException, IllegalAccessException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    solrDocumentList.setNumFound(10);
    ActiveProductsRequestVO activeProductsRequestVOCNCTrue = new ActiveProductsRequestVO();
    BeanUtils.copyProperties(activeProductsRequestVOCNCTrue, activeProductsRequestVO);
    activeProductsRequestVOCNCTrue.setPickupPointCodes(pickupCodes);
    activeProductsRequestVOCNCTrue.setCncActivated(Boolean.TRUE);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVOCNCTrue, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse, times(3)).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(1, result.getContent().size());
    Assertions.assertEquals(10L, result.getNumGroups());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(result.getNumGroups(), 10);
  }

  @Test
  public void getActiveProductsListForMerchantEmptyResultTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(solrQueryResponse);
    when(solrQueryResponse.getResults()).thenReturn(solrDocumentList);
    SolrGroupResultVO<ProductSolr> result =
        this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Mockito.verify(this.solrQueryResponse).getResults();
    Assertions.assertEquals(pageable.getPageNumber(), result.getPageNumber().intValue());
    Assertions.assertEquals(pageable.getPageSize(), result.getPageSize().intValue());
    Assertions.assertEquals(result.getContent().size(), 0);
  }

  @Test
  public void getActiveProductsListForMerchantExceptionTest() throws IOException, SolrServerException {
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->
          this.productAndItemSolrRepository.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable));
    } finally {
      Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getProductsForOfficialStoreSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    SolrGroupResultVO<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsForOfficialStoreSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    SolrGroupResultVO<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsForOfficialStoreExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    SolrGroupResultVO<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs =
          this.productAndItemSolrRepository.getProductsForOfficialStore(STORE_ID, officialStoreRequestVO, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsForOfficialStoreL3Test() throws Exception {
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository.getProductsByCategoryAndMerchantCodeL3(STORE_ID, officialStoreRequestVO,
        PageRequest.of(0, 10));
    Mockito.verify(cloudSolrClientL3).query(Mockito.any(SolrQuery.class));
  }

  @Test
  public void getProductsForOfficialStoreL3ExceptionTest() throws Exception {
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () ->
          productAndItemSolrRepository.getProductsByCategoryAndMerchantCodeL3(STORE_ID, officialStoreRequestVO,
              PageRequest.of(0, 10)));
    } finally {
      Mockito.verify(cloudSolrClientL3).query(Mockito.any(SolrQuery.class));
    }
  }

  @Test
  public void getListItemsByProductSkusTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        productAndItemSolrRepository.getListItemsByProductSkus(STORE_ID, stringList, officialStoreRequestVO, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(pageable.getPageNumber(), result.getNumber());
    assertEquals(pageable.getPageSize(), result.getSize());
    assertEquals(productAndItemSolr, result.getContent().get(0));
  }

  @Test
  public void getListItemsByProductSkusExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.getListItemsByProductSkus(STORE_ID, stringList, officialStoreRequestVO, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getListItemsByProductSkusSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.getListItemsByProductSkus(STORE_ID, stringList, officialStoreRequestVO, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getListItemsByProductSkusIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.getListItemsByProductSkus(STORE_ID, stringList, officialStoreRequestVO, pageable);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getProductsByMasterCatalog() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMasterCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Boolean.FALSE, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(solrQueryArgumentCaptor.getValue().getQuery(),
        SolrFieldNames.MASTER_CATALOG + SolrConstants.COLON + SolrConstants.QUOTES + CATALOG_CODE + SOLR_DELIMITER
            + CATEGORY_CODE + SolrConstants.QUOTES);
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMasterCatalogSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsByMasterCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Boolean.FALSE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsByMasterCatalogSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsByMasterCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Boolean.FALSE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsByMasterCatalogExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsByMasterCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, Boolean.TRUE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }


  @Test
  public void getProductsBySalesCatalog() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.getProductsBySalesCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(solrQueryArgumentCaptor.getValue().getQuery(),
        SolrFieldNames.SALES_CATALOG + SolrConstants.COLON + SolrConstants.QUOTES + CATALOG_CODE + SOLR_DELIMITER
            + CATEGORY_CODE + SolrConstants.QUOTES);
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsBySalesCatalogSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsBySalesCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsBySalesCatalogSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsBySalesCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsBySalesrCatalogExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsBySalesCatalog(STORE_ID, CATALOG_CODE, CATEGORY_CODE, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithNullSalesCatalogAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsWithNullSalesCatalogAndMarkForDeleteFalse(STORE_ID, new DateTime(), new DateTime(), pageable);
    Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsWithNullSalesCatalogAndMarkForDeleteFalseSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithNullSalesCatalogAndMarkForDeleteFalse(STORE_ID, new DateTime(), new DateTime(),
              PageRequest.of(0, 10));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithNullSalesCatalogAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithNullSalesCatalogAndMarkForDeleteFalse(STORE_ID, new DateTime(), new DateTime(),
              PageRequest.of(0, 10));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithNullSalesCatalogAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithNullSalesCatalogAndMarkForDeleteFalse(STORE_ID, new DateTime(), new DateTime(),
              PageRequest.of(0, 10));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse()
      throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class), Mockito.any(SolrRequest.METHOD.class)))
        .thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
            Arrays.asList(CATEGORY_CODE), Arrays.asList(BRAND), ITEM_SKU, ITEM_SKU, PAGE_REQUEST);
    Mockito.verify(this.cloudSolrClient)
        .query(solrQueryArgumentCaptor.capture(), Mockito.any(SolrRequest.METHOD.class));
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(pageable.getPageNumber(), result.getNumber());
    Assertions.assertEquals(pageable.getPageSize(), result.getSize());
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalseSolrServerExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class), Mockito.any(SolrRequest.METHOD.class)))
        .thenThrow(SolrServerException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Arrays.asList(CATEGORY_CODE), Arrays.asList(BRAND), null, ITEM_SKU, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient)
          .query(solrQueryArgumentCaptor.capture(), Mockito.any(SolrRequest.METHOD.class));
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalseSolrExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class), Mockito.any(SolrRequest.METHOD.class)))
        .thenThrow(SolrException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Arrays.asList(CATEGORY_CODE), Arrays.asList(BRAND), ITEM_SKU, ITEM_SKU, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient)
          .query(solrQueryArgumentCaptor.capture(), Mockito.any(SolrRequest.METHOD.class));
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalseExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class), Mockito.any(SolrRequest.METHOD.class)))
        .thenThrow(IOException.class);
    Page<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Arrays.asList(CATEGORY_CODE), Arrays.asList(BRAND), ITEM_SKU, ITEM_SKU, PAGE_REQUEST);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient)
          .query(solrQueryArgumentCaptor.capture(), Mockito.any(SolrRequest.METHOD.class));
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void deleteOfflineItemByMerchantCode() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenReturn(new UpdateResponse());
    this.productAndItemSolrRepository.deleteOfflineItemByItemIds(Arrays.asList(ID));
    Mockito.verify(this.cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
  }

  @Test
  public void deleteOfflineItemByMerchantCodeSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList()))
        .thenThrow(SolrServerException.class);
    try {
      this.productAndItemSolrRepository.deleteOfflineItemByItemIds(Arrays.asList(ID));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
    }
  }

  @Test
  public void deleteOfflineItemByMerchantCodeSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenThrow(SolrException.class);
    try {
      this.productAndItemSolrRepository.deleteOfflineItemByItemIds(Arrays.asList(ID));

    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
    }
  }

  @Test
  public void deleteOfflineItemByMerchantCodeExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList())).thenThrow(IOException.class);
    try {
      this.productAndItemSolrRepository.deleteOfflineItemByItemIds(Arrays.asList(ID));

    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
    }
  }

  @Test
  public void findByProductCode() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository.findByProductCode(SolrFieldNames.PRODUCT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByProductCodeSolrMaxRowSize() throws IOException, SolrServerException {
    ReflectionTestUtils.setField(productAndItemSolrRepository, "solrMaxRowSize", Integer.valueOf(1));
    queryResponse.getResults().setNumFound(2);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository.findByProductCode(SolrFieldNames.PRODUCT_CODE);
    Mockito.verify(this.cloudSolrClient, times(2)).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(result.size(), 2);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByProductSku() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository.findByProductSku(SolrFieldNames.PRODUCT_SKU);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByProductSkuSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository.findByProductSku(SolrFieldNames.PRODUCT_SKU);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByProductCodeSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository.findByProductCode(SolrFieldNames.PRODUCT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByProductCodeSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository.findByProductCode(SolrFieldNames.PRODUCT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByProductCodeExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository.findByProductCode(SolrFieldNames.PRODUCT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse()
      throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Set<ProductAndItemSolr> productAndItemSolrSet = this.productAndItemSolrRepository
        .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
            Boolean.FALSE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(productAndItemSolrSet.size(), 1);
    Assertions.assertEquals(ID, productAndItemSolrSet.stream().findFirst().get().getId());
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalseSolrServerExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Set<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Boolean.FALSE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalseSolrExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    Set<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Boolean.FALSE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    Set<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
              Boolean.FALSE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalseSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    queryResponse.getResults().setNumFound(1L);
    Long result = this.productAndItemSolrRepository.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result, 1L, 0);
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalseNullResultsTest() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(new QueryResponse());
    Long result = this.productAndItemSolrRepository.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result, 0L, 0);
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () -> this.productAndItemSolrRepository.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUPPOINT_CODE));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(STORE_ID,
            new HashSet<>(Arrays.asList(CATEGORY_CODE)));
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByStoreIdAndEmptyProductCatentryIdInAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(STORE_ID,
            new HashSet<>(Collections.EMPTY_LIST));
    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalseSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(STORE_ID,
              new HashSet<>(Arrays.asList(CATEGORY_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(STORE_ID,
              new HashSet<>(Arrays.asList(CATEGORY_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(STORE_ID,
              new HashSet<>(Arrays.asList(CATEGORY_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue()
      throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, PRODUCT_CODE,
            MERCHANT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.PRODUCT_CODE + SolrConstants.COLON + PRODUCT_CODE,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void
  findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrueSolrServerExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, PRODUCT_CODE,
              MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrueSolrExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, PRODUCT_CODE,
              MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrueExceptionTest()
      throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeAndMerchantCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, PRODUCT_CODE,
              MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    Assertions.assertEquals(result.size(), 1);
    Assertions.assertEquals(ID, result.get(0).getId());
  }

  @Test
  public void findByStoreIdAndEmptyProductCodeInAndMarkForDeleteFalse() throws IOException, SolrServerException {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Collections.EMPTY_LIST));
    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseSolrServerExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(IOException.class);
    List<ProductAndItemSolr> productAndItemSolrs = null;
    try {
      productAndItemSolrs = this.productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalse(STORE_ID, new HashSet<>(Arrays.asList(PRODUCT_CODE)));
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      Assertions.assertNull(productAndItemSolrs);
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = productAndItemSolrRepository
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, setList);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findByStoreIdAndEmptyProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = productAndItemSolrRepository
        .findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, Collections.EMPTY_SET);
    assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrueExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      assertTrue(filterQueries.contains(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE));
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrueIOExceptionTest()
      throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      assertTrue(filterQueries.contains(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE));
    }
  }

  @Test
  public void findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrueSolrExceptionTest()
      throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findByStoreIdAndProductCodeInAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      assertTrue(filterQueries.contains(SolrFieldNames.IS_SYNCHRONIZED + SolrConstants.COLON + Boolean.TRUE));
    }
  }

  @Test
  public void findByStoreIdAndProductSku() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = productAndItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
      MERCHANT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findByStoreIdAndProductSkuExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));    }
  }

  @Test
  public void findByStoreIdAndProductSkuIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));    }
  }

  @Test
  public void findByStoreIdAndProductSkuSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));    }
  }

  @Test
  public void findByStoreIdAndProductSkuAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
          MERCHANT_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
        .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findByStoreIdAndProductSkuAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuAndMarkForDeleteFalseIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        MERCHANT_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(solrQueryArgumentCaptor.getValue().getQuery()
          .contains(SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + PRODUCT_SKU));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, stringList);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findByStoreIdAndEmptyProductSkuInAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = productAndItemSolrRepository
        .findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Collections.EMPTY_LIST);
    assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, stringList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalseIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, stringList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, stringList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalse1() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, setList);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findByStoreIdAndEmptyProductSkuSetInAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Collections.EMPTY_SET);
    assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndEmptyProductSkuInAndMarkForDeleteFalse1() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Collections.EMPTY_LIST);
    assertEquals(result.size(), 0);
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalse1ExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalse1IOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByStoreIdAndProductSkuInAndMarkForDeleteFalse1SolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findByTicketTemplateCodeAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findByTicketTemplateCodeAndMarkForDeleteFalse(TICKET_TEMPLATE_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    assertEquals(
        SolrFieldNames.TICKET_TEMPLATE_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE + TICKET_TEMPLATE_CODE
            + SolrConstants.DOUBLE_QUOTE, solrQueryArgumentCaptor.getValue().getQuery());
  }

  @Test
  public void findByTicketTemplateCodeAndMarkForDeleteFalseExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByTicketTemplateCodeAndMarkForDeleteFalse(TICKET_TEMPLATE_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      assertEquals(
          SolrFieldNames.TICKET_TEMPLATE_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE + TICKET_TEMPLATE_CODE
              + SolrConstants.DOUBLE_QUOTE, solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findByTicketTemplateCodeAndMarkForDeleteFalseIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByTicketTemplateCodeAndMarkForDeleteFalse(TICKET_TEMPLATE_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      assertEquals(
          SolrFieldNames.TICKET_TEMPLATE_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE + TICKET_TEMPLATE_CODE
              + SolrConstants.DOUBLE_QUOTE, solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findByTicketTemplateCodeAndMarkForDeleteFalseSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findByTicketTemplateCodeAndMarkForDeleteFalse(TICKET_TEMPLATE_CODE);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      assertEquals(SolrFieldNames.TICKET_TEMPLATE_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTE + TICKET_TEMPLATE_CODE
              + SolrConstants.DOUBLE_QUOTE,
          solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    ProductAndItemSolr result = productAndItemSolrRepository
        .findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CATENTRY_ID);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + PRODUCT_CATENTRY_ID,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(productAndItemSolr, result);
  }

  @Test
  public void findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalseException() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CATENTRY_ID);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + PRODUCT_CATENTRY_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalseIOException() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CATENTRY_ID);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + PRODUCT_CATENTRY_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalseSolrException() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository
          .findFirstByStoreIdAndProductCatentryIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_CATENTRY_ID);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.PRODUCT_CATENTRY_ID + SolrConstants.COLON + PRODUCT_CATENTRY_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    }
  }

  @Test
  public void findItemSkuAndCodeByStoreIdAndItemSkuIn() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result =
        productAndItemSolrRepository.findItemSkuAndCodeByStoreIdAndItemSkuIn(STORE_ID, setList);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(productAndItemSolr, result.get(0));
  }

  @Test
  public void findItemSkuAndCodeByStoreIdAndItemSkuInExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findItemSkuAndCodeByStoreIdAndItemSkuIn(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findItemSkuAndCodeByStoreIdAndItemSkuInIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findItemSkuAndCodeByStoreIdAndItemSkuIn(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findItemSkuAndCodeByStoreIdAndItemSkuInSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findItemSkuAndCodeByStoreIdAndItemSkuIn(STORE_ID, setList);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    }
  }

  @Test
  public void findFirstByStoreIdAndItemSku() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    ProductAndItemSolr result = productAndItemSolrRepository.findFirstByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + ITEM_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(productAndItemSolr, result);
  }

  @Test
  public void findFirstByStoreIdAndItemSkuExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findFirstByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + ITEM_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findFirstByStoreIdAndItemSkuIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findFirstByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + ITEM_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findFirstByStoreIdAndItemSkuSolrExceptionTest() throws Exception {
    Mockito.doThrow(SolrException.class).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      productAndItemSolrRepository.findFirstByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    } catch (SolrCustomException e) {
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertEquals(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + ITEM_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    List<ProductAndItemSolr> result = productAndItemSolrRepository.
        findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE, solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertEquals(productAndItemsSolr, result);
  }

  @Test
  public void updateCncActivatedByProductSkuSetSolrViaEventTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    ReflectionTestUtils.setField(productAndItemSolrRepository, "eventBasedSolrUpdateEnable", true);
    this.productAndItemSolrRepository.updateCncActivatedByProductSkuSetSolr(STORE_ID, productSkuSet, Boolean.FALSE,
      MERCHANT_CODE);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void updateCncActivatedByProductSkuSetSolrTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    this.productAndItemSolrRepository.updateCncActivatedByProductSkuSetSolr(STORE_ID, productSkuSet, Boolean.FALSE,
      MERCHANT_CODE);
    verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    verify(cloudSolrClientL3).add(Mockito.anyList());
    assertEquals(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + SolrConstants.DOUBLE_QUOTE
            + PRODUCT_SKU + SolrConstants.DOUBLE_QUOTE + SolrConstants.CLOSING_BRACKET,
        solrQueryArgumentCaptor.getValue().getQuery());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
  }

  @Test
  public void updateCncActivatedByProductSkuSetSolrCustomNumFoundTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    queryResponse.getResults().setNumFound(11);
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    this.productAndItemSolrRepository.updateCncActivatedByProductSkuSetSolr(STORE_ID, productSkuSet, Boolean.FALSE,
      MERCHANT_CODE);
    verify(cloudSolrClientL3, times(2)).query(solrQueryArgumentCaptor.capture());
    verify(cloudSolrClientL3, times(2)).add(Mockito.anyList());
    assertEquals(
        SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + SolrConstants.DOUBLE_QUOTE
            + PRODUCT_SKU + SolrConstants.DOUBLE_QUOTE + SolrConstants.CLOSING_BRACKET,
        solrQueryArgumentCaptor.getValue().getQuery());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
  }

  @Test
  public void updateCncActivatedByProductSkuSetSolrExceptionTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Exception exception = null;
    try {
      this.productAndItemSolrRepository.updateCncActivatedByProductSkuSetSolr(STORE_ID, productSkuSet, Boolean.FALSE,
        MERCHANT_CODE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(SolrCustomException.class, exception.getClass());
      verify(cloudSolrClientL3).query(Mockito.any());
    }
  }

  @Test
  public void updateCncActivatedByMerchantCodeSolrTest() throws Exception {
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    this.productAndItemSolrRepository.updateCncActivatedByMerchantCodeSolr(STORE_ID, MERCHANT_CODE, Boolean.FALSE);
    verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    verify(cloudSolrClientL3).add(Mockito.anyList());
    assertEquals(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE,
        solrQueryArgumentCaptor.getValue().getQuery());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
  }

  @Test
  public void updateCncActivatedByMerchantCodeSolrViaEventTest() throws Exception {
    ReflectionTestUtils.setField(productAndItemSolrRepository, "eventBasedSolrUpdateEnable", true);
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    this.productAndItemSolrRepository.updateCncActivatedByMerchantCodeSolr(STORE_ID, MERCHANT_CODE, Boolean.FALSE);
    verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR),
      eq(PRODUCT_SKU),productAndItemEventModel.capture());
    Assertions.assertEquals(productAndItemEventModel.getValue().getProductSku(), PRODUCT_SKU);
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
  }

  @Test
  public void updateCncActivatedByMerchantCodeSolrCustomNumFoundTest() throws Exception {
    queryResponse.getResults().setNumFound(11);
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    this.productAndItemSolrRepository.updateCncActivatedByMerchantCodeSolr(STORE_ID, MERCHANT_CODE, Boolean.FALSE);
    verify(cloudSolrClientL3, times(2)).query(solrQueryArgumentCaptor.capture());
    verify(cloudSolrClientL3, times(2)).add(Mockito.anyList());
    assertEquals(SolrFieldNames.MERCHANT_CODE + SolrConstants.COLON + MERCHANT_CODE,
        solrQueryArgumentCaptor.getValue().getQuery());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
  }

  @Test
  public void updateCncActivatedByMerchantCodeSolrExceptionTest() throws Exception {
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Exception exception = null;
    try {
      this.productAndItemSolrRepository.updateCncActivatedByMerchantCodeSolr(STORE_ID, MERCHANT_CODE, Boolean.FALSE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(SolrCustomException.class, exception.getClass());
      verify(cloudSolrClientL3).query(Mockito.any());
    }
  }

  @Test
  public void save() throws Exception {
    productAndItemSolrRepository.save(productAndItemSolr);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    assertEquals(ID, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.ID).getValue());
    assertEquals(ITEM_SKU, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.ITEM_SKU).getValue());
    assertEquals(ITEM_CODE, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.ITEM_CODE).getValue());
    assertEquals(PRODUCT_CODE, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.PRODUCT_CODE).getValue());
    assertEquals(PRODUCT_SKU, solrInputDocumentArgumentCaptor.getValue().get(SolrFieldNames.PRODUCT_SKU).getValue());
  }

  @Test
  public void saveSolrServerException() throws Exception {
    when(cloudSolrClient.add(any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
    Exception exception = null;
    try {
      productAndItemSolrRepository.save(productAndItemSolr);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(SolrCustomException.class, exception.getClass());
      verify(this.cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    }
  }

  @Test
  public void findOne() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ProductAndItemSolr result = productAndItemSolrRepository.findOne(ITEM_SKU, MERCHANT_CODE);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertEquals(SolrFieldNames.ITEM_SKU + SolrConstants.COLON + ITEM_SKU, solrQueryArgumentCaptor.getValue().getQuery());
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(productAndItemSolr, result);
  }

  @Test
  public void findOneSolrServerException() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    Exception exception = null;
    try {
      productAndItemSolrRepository.findOne(ITEM_SKU, StringUtils.EMPTY);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(SolrCustomException.class, exception.getClass());
      verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getItemSkuByMerchantCodeAndCreatedDateLessThanAndMarkForDeleteFalseTest()
      throws IOException, SolrServerException {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> productAndItemSolrs =
        productAndItemSolrRepository.getItemsByMerchantCode(
            STORE_ID,
            MERCHANT_CODE, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 3);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
  }

  @Test
  public void getItemSkuByMerchantCodeAndCreatedDateLessThanAndMarkForDeleteFalseTest_WhenResponseIsNull()
      throws IOException, SolrServerException {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(null);
    Page<ProductAndItemSolr> productAndItemSolrs =
        productAndItemSolrRepository.getItemsByMerchantCode(
            STORE_ID,
            MERCHANT_CODE, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 3);
    assertTrue(CollectionUtils.isEmpty(productAndItemSolrs.getContent()));
  }

  @Test
  public void getItemSkuByMerchantCodeAndCreatedDateLessThanAndMarkForDeleteFalseTest_WhenException()
      throws IOException, SolrServerException {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      productAndItemSolrRepository.getItemsByMerchantCode(
          STORE_ID,
          MERCHANT_CODE, PAGE_REQUEST);
    } catch (SolrCustomException scex) {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 3);
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenSkuStateChangeTrueTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    this.productAndItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
    assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    this.productAndItemSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
    verify(cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
    assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.ID));
    assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.IS_SYNCHRONIZED));
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateSolrServerExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    when(cloudSolrClient.add(anyList())).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrServerException.class, () -> this.productAndItemSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument)));
    } finally {
      verify(cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.ID));
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.IS_SYNCHRONIZED));
    }
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateSolrExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    when(cloudSolrClient.add(anyList())).thenThrow(SolrException.class);
    try {
      Assertions.assertThrows(SolrException.class, () -> this.productAndItemSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument)));
    } finally {
      verify(cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.ID));
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.IS_SYNCHRONIZED));
    }
  }

  @Test
  public void executeSolrDocumentsAtomicUpdateRouteExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.IS_SYNCHRONIZED, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    when(cloudSolrClient.add(anyList())).thenThrow(CloudSolrClient.RouteException.class);
    try {
      Assertions.assertThrows(CloudSolrClient.RouteException.class, () -> this.productAndItemSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument)));
    } finally {
      verify(cloudSolrClient).add(solrDocumentListArgumentCaptor.capture());
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.ID));
      assertTrue(solrDocumentListArgumentCaptor.getValue().get(0).containsKey(SolrFieldNames.IS_SYNCHRONIZED));
    }
  }

  @Test
  public void indexMerchantPromoDiscountItem_whenExceptionTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(ProductFieldNames.ID, ITEM_SKU);
    solrInputDocument.setField(ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true);
    solrInputDocument.setField(ProductFieldNames.UPDATED_DATE, new Date());
    when(cloudSolrClient.add(any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrServerException.class, () -> this.productAndItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument));
    } finally {
      verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.ID));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
      assertTrue(solrInputDocumentArgumentCaptor.getValue().containsKey(SolrFieldNames.UPDATED_DATE));

    }
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(4, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusExceptionTest() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(new IOException());
    try {
      this.productAndItemSolrRepository
          .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    } catch (Exception e) {
      Mockito.verify(systemParameterService, times(2))
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertEquals(4, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    }
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withActiveStatus() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setStatus(ACTIVE);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(4, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withSuspendedStatus() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setStatus(SUSPENDED);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(4, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withNullMerchantCode() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setMerchantCode(null);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withNullCategoryCodes()
      throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setCategoryCodes(null);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withNullMerchantAndCategoryCodes() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setCategoryCodes(null);
    activeProductsRequestVO.setMerchantCode(null);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withNullMerchantAndCategoryCodesActiveStatus() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setCategoryCodes(null);
    activeProductsRequestVO.setMerchantCode(null);
    activeProductsRequestVO.setStatus(ACTIVE);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusTest_withNullMerchantAndCategoryCodesSuspendedStatus() throws IOException, SolrServerException {
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION, "false", ""));
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    activeProductsRequestVO.setCategoryCodes(null);
    activeProductsRequestVO.setMerchantCode(null);
    activeProductsRequestVO.setStatus(SUSPENDED);
    Page<ProductAndItemSolr> result = this.productAndItemSolrRepository
        .getProductsByMerchantCodeAndCategoryCodesAndStatus(STORE_ID, activeProductsRequestVO, pageable);
    Mockito.verify(systemParameterService, times(2))
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_XPRODUCT_L3_COLLECTION);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(result.getContent().size(), 1);
    Assertions.assertEquals(ID, result.getContent().get(0).getId());
  }

  @Test
  public void getItemsByProductSkusTest() throws IOException, SolrServerException {
    queryResponse.setResponse(facetCount);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Map<String,Long>  result = this.productAndItemSolrRepository.getItemsByProductSkus(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(1, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  public void getItemsByProductSkusExceptionTest() throws IOException, SolrServerException {
    Mockito.doThrow(new IOException()).when(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
    try {
      this.productAndItemSolrRepository.getItemsByProductSkus(STORE_ID, Arrays.asList(PRODUCT_SKU));
    } catch (Exception e) {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      Assertions.assertEquals(1, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
    }
  }

  @Test
  public void getItemsByProductSkusTest_withEmptyList() throws IOException, SolrServerException {
    Map<String, Long> result =
        this.productAndItemSolrRepository.getItemsByProductSkus(STORE_ID, new ArrayList<>());
    Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodes_WhenResponseIsNull() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(null);
    Page<ProductAndItemSolr> productAndItemSolrs =
        productAndItemSolrRepository.getItemsByMerchantCodeAndCategoryCodes(STORE_ID, new ActiveProductsRequestVO(), PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertTrue(CollectionUtils.isEmpty(productAndItemSolrs.getContent()));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodes_WhenException()
      throws  Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      productAndItemSolrRepository
          .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, new ActiveProductsRequestVO(), PAGE_REQUEST);
    } catch (SolrCustomException scex) {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
  }

  @Test
  public void findProductsByBrandTest() throws Exception {
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenReturn(queryResponse);
    Long result = productAndItemSolrRepository.findByStoreIdAndBrandAndMarkForDeleteFalse(STORE_ID, BRAND);
    verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 2);
    assertTrue(solrQuery.toString().contains(BRAND_QUOTES));
    Assertions.assertNotNull(result);
  }

  @Test
  public void findProductsByBrandExceptionTest() throws IOException, SolrServerException {
    when(cloudSolrClientL3.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Long result = productAndItemSolrRepository.findByStoreIdAndBrandAndMarkForDeleteFalse(STORE_ID, BRAND);
    } catch (SolrCustomException scex) {
      verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    }
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 2);
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusAll() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("ALL");
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusActive() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("ACTIVE");
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusSuspended() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("SUSPENDED");
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusAll_withCategoryCodes()
    throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("ALL");
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusAll_sortTypeDesc() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("ALL");
    activeProductsRequestVO.setSortType("desc");
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void getItemsByMerchantCodeAndCategoryCodesTest_statusAll_sortTypeAsc() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenReturn(queryResponse);
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setNameKey("key");
    activeProductsRequestVO.setSearchKey("key");
    activeProductsRequestVO.setPickupPointCodes(Arrays.asList(PICKUP_POINT));
    activeProductsRequestVO.setStatus("ALL");
    activeProductsRequestVO.setSortType("Asc");
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository
        .getItemsByMerchantCodeAndCategoryCodes(STORE_ID, activeProductsRequestVO, PAGE_REQUEST);
    verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    SolrQuery solrQuery = solrQueryArgumentCaptor.getValue();
    assertEquals(solrQuery.getFilterQueries().length, 1);
    assertEquals(productAndItemSolrs.getContent().size(), 1);
    assertTrue(solrQuery.getQuery()
        .contains(SolrConstants.OPEN_BRACKET + SolrFieldNames.PICKUP_POINT_CODE + SolrConstants.COLON));
  }

  @Test
  public void findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse_exceptionTest() throws Exception {
    when(cloudSolrClient.query(any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () -> productAndItemSolrRepository.
          findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE));
    } finally {
      verify(cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    }
  }

  @Test
  public void getItemSkusByPristineIdTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Set<String> result = productAndItemSolrRepository.getItemSkusByPristineId(STORE_ID, PRISTINE_ID);
    Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(
        SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + SolrConstants.QUOTES + PRISTINE_ID + SolrConstants.QUOTES,
        solrQueryArgumentCaptor.getValue().getQuery());
  }

  @Test
  public void getItemSkusByPristineIdGreaterThanSolrMaxSizeTest() throws Exception {
    queryResponse.getResults().setNumFound(20);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    Set<String> result = productAndItemSolrRepository.getItemSkusByPristineId(STORE_ID, PRISTINE_ID);
    Mockito.verify(this.cloudSolrClient, times(2)).query(solrQueryArgumentCaptor.capture());
    List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getAllValues().get(0).getFilterQueries());
    assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
    assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
    assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
    assertEquals(
        SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + SolrConstants.QUOTES + PRISTINE_ID + SolrConstants.QUOTES,
        solrQueryArgumentCaptor.getValue().getQuery());
    assertEquals(Integer.valueOf(10), solrQueryArgumentCaptor.getAllValues().get(1).getStart());
    assertEquals(Integer.valueOf(10), solrQueryArgumentCaptor.getAllValues().get(1).getRows());
  }

  @Test
  public void getItemSkusByPristineIdExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () -> productAndItemSolrRepository.getItemSkusByPristineId(STORE_ID, PRISTINE_ID));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQueryArgumentCaptor.capture());
      List<String> filterQueries = Arrays.asList(solrQueryArgumentCaptor.getValue().getFilterQueries());
      assertTrue(filterQueries.contains(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID));
      assertTrue(filterQueries.contains(SolrFieldNames.MARK_FOR_DELETE + SolrConstants.COLON + Boolean.FALSE));
      assertTrue(filterQueries.contains(SolrFieldNames.IS_ARCHIVED + SolrConstants.COLON + Boolean.FALSE));
      assertEquals(
          SolrFieldNames.PRISTINE_ID + SolrConstants.COLON + SolrConstants.QUOTES + PRISTINE_ID + SolrConstants.QUOTES,
          solrQueryArgumentCaptor.getValue().getQuery());
    }
  }

  @Test
  public void getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalseTest() throws IOException, SolrServerException {
    List<List<String>> categoryInBatches = new ArrayList<>();
    categoryInBatches.add(Arrays.asList(CATEGORY_CODE));
    categoryInBatches.add(Arrays.asList(CATEGORY_CODE));
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository.getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(STORE_ID,
        categoryInBatches, Arrays.asList(BRAND), MERCHANT_CODE);
    Mockito.verify(cloudSolrClientL3, times(2)).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalsL5CountNotPresentTest()
      throws IOException, SolrServerException {
    SolrDocumentList solrDocuments = new SolrDocumentList();
    solrDocuments.add(solrDocument);
    NamedList<Object> response = new NamedList<>();
    NamedList<Object> response1 = new NamedList<>();
    nullQueryResponse = new QueryResponse();
    response.add("response", solrDocuments);
    NamedList namedList = new NamedList();
    response1.add("facets", namedList);
    queryResponse = new QueryResponse();
    queryResponse.setResponse(response1);
    List<List<String>> categoryInBatches = new ArrayList<>();
    categoryInBatches.add(Arrays.asList(CATEGORY_CODE));
    categoryInBatches.add(Arrays.asList(CATEGORY_CODE));
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository.getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(STORE_ID,
        categoryInBatches, Arrays.asList(BRAND), MERCHANT_CODE);
    Mockito.verify(cloudSolrClientL3, times(2)).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalseTest_null() throws IOException, SolrServerException {
    List<List<String>> categoryInBatches = new ArrayList<>();
    categoryInBatches.add(null);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository
        .getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(STORE_ID, categoryInBatches, null, MERCHANT_CODE);
    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2Test() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        new ActiveProductsRequestVO(), PAGE_REQUEST);
    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertTrue(ArrayUtils.isEmpty(solrQueryArgumentCaptor.getValue().getFilterQueries()));
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2SuspendTest()
      throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO1 = new ActiveProductsRequestVO();
    activeProductsRequestVO1.setStatus(SUSPENDED);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        activeProductsRequestVO1, PAGE_REQUEST);
    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertFalse(ArrayUtils.isEmpty(solrQueryArgumentCaptor.getValue().getFilterQueries()));
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2NullResponseTest() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    Page<ProductAndItemSolr> productAndItemSolrs = productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        new ActiveProductsRequestVO(), PAGE_REQUEST);
    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
        solrQueryArgumentCaptor.getValue().getQuery());
    Assertions.assertTrue(ArrayUtils.isEmpty(solrQueryArgumentCaptor.getValue().getFilterQueries()));
    Assertions.assertTrue(productAndItemSolrs.getContent().isEmpty());
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2ErrorTest() throws SolrServerException, IOException {
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(SolrCustomException.class, () -> productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
          new ActiveProductsRequestVO(), PAGE_REQUEST));
    } finally {
      Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
      Assertions.assertEquals(SolrFieldNames.STORE_ID + SolrConstants.COLON + STORE_ID,
          solrQueryArgumentCaptor.getValue().getQuery());
      Assertions.assertTrue(ArrayUtils.isEmpty(solrQueryArgumentCaptor.getValue().getFilterQueries()));
    }
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2AllFiltersTest() throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setStatus(ALL);
    activeProductsRequestVO.setSearchKey(PRODUCT_CODE);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        activeProductsRequestVO, PAGE_REQUEST);

    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2ProductNameSearchTest() throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setStatus(ALL);
    activeProductsRequestVO.setSearchKey(PRODUCT_NAME);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
      activeProductsRequestVO, PAGE_REQUEST);

    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2ActiveFiltersTest() throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setStatus(ACTIVE);
    activeProductsRequestVO.setSearchKey(PRODUCT_CODE);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        activeProductsRequestVO, PAGE_REQUEST);

    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2SuspendFiltersTest() throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setStatus(ACTIVE);
    activeProductsRequestVO.setSearchKey(PRODUCT_CODE);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        activeProductsRequestVO, PAGE_REQUEST);

    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(3, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
  }

  @Test
  public void getProductsByMerchantCodeAndCategoryCodesAndStatusV2WrongFiltersTest() throws SolrServerException, IOException {
    ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
    activeProductsRequestVO.setMerchantCode(MERCHANT_CODE);
    activeProductsRequestVO.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    activeProductsRequestVO.setStatus(PRODUCT_CODE);
    activeProductsRequestVO.setSearchKey(PRODUCT_CODE);
    Mockito.when(cloudSolrClientL3.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);

    productAndItemSolrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(STORE_ID,
        activeProductsRequestVO, PAGE_REQUEST);

    Mockito.verify(cloudSolrClientL3).query(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(2, solrQueryArgumentCaptor.getValue().getFilterQueries().length);
  }

}
