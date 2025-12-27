package com.gdn.x.product.dao.solr.impl;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.util.NamedList;
import org.apache.solr.common.util.SimpleOrderedMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.service.api.ItemService;

public class SolrIndexingRepositoryBeanTest {

  @InjectMocks
  private SolrIndexingRepositoryBean solrIndexingRepositoryBean;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private SolrClient sourceClient;

  @Mock
  private SolrClient destinationClient;

  @Mock
  private NamedList namedList;

  @Mock
  private Map<String, Object> map;

  @Mock
  private UpdateResponse updateResponse;

  @Mock
  private ItemService itemService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQueryCaptor;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> listArgumentCaptor;
  private static final String QUERY = "id:*";
  private static final String ITEM_IMAGE = "true#_#image1";
  private static final String IMAGE = "image1";
  private static final String MASTER_CATLOG_QUERY = "masterCatalog:(\"masterCatalog\")";
  private static final String DEFAULT_STORE_ID = "10001";

  private List<SolrInputDocument> solrInputDocuments;
  private SolrInputDocument solrInputDocument;
  private List<SimpleOrderedMap> maps;
  private SimpleOrderedMap simpleOrderedMap;
  private SolrDocumentList solrDocumentList;
  private SolrDocumentList solrDocumentList1;
  private Map<String, ProductAndItemsVO> productSkuAndProductAndItemsVOMap = new HashMap<>();
  private Product product = new Product();
  private Item item = new Item();
  private InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    solrDocument.addField(SolrFieldNames.ITEM_IMAGES, Arrays.asList(ITEM_IMAGE));
    solrDocument.addField(SolrFieldNames.PRODUCT_SKU, SolrFieldNames.PRODUCT_SKU);
    solrDocument.addField(SolrFieldNames.MASTER_CATALOG, SolrFieldNames.MASTER_CATALOG);
    solrDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.MARK_FOR_DELETE, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    solrDocument.setField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    solrDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonList(SolrFieldNames.SALES_CATALOG));
    solrDocument.setField(SolrFieldNames.MERCHANT_CODE, SolrFieldNames.MERCHANT_CODE);
    solrDocument.setField(SolrFieldNames.IS_SUSPENDED, Boolean.FALSE);
    solrDocument.setField(SolrFieldNames.STORE_ID, SolrFieldNames.STORE_ID);
    solrDocument.setField(SolrFieldNames.CREATED_DATE, new Date());
    solrDocument.setField(SolrFieldNames.UPDATED_DATE, new Date());
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    simpleOrderedMap = new SimpleOrderedMap();
    simpleOrderedMap.add(SolrConstants.NAME, SolrFieldNames.PRODUCT_NAME);
    simpleOrderedMap.add(SolrConstants.VERSION, Constants.NOT_APPLICABLE);
    solrInputDocuments = new ArrayList<>();
    solrInputDocument = new SolrInputDocument();
    solrInputDocument.addField(SolrFieldNames.PRODUCT_NAME, Constants.DEFAULT_USERNAME);
    solrInputDocuments.add(solrInputDocument);
    maps = new ArrayList<>();
    maps.add(simpleOrderedMap);
    product.setProductSku(SolrFieldNames.PRODUCT_SKU);
    item.setProductSku(SolrFieldNames.PRODUCT_SKU);
    item.setItemSku(SolrFieldNames.ITEM_SKU);
    item.setPickupPointCode(SolrFieldNames.PICKUP_POINT_CODE);
    productSkuAndProductAndItemsVOMap
        .put(SolrFieldNames.PRODUCT_SKU, new ProductAndItemsVO(product, Arrays.asList(item)));
    inventoryStockInfoDTO.setWebProductSku(SolrFieldNames.PRODUCT_SKU);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(queryResponse, map, destinationClient, sourceClient, itemService);
    verifyNoMoreInteractions(this.inventoryOutbound);
  }

  @Test
  public void updateAllTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(maps);
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllCopyFieldsAndDynamicFieldsTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllStatusNonZeroTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(1);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllResponseExceptionTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenThrow(RuntimeException.class);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
  }

  @Test
  public void updateAllExceptionTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.namedList.get(SolrConstants.SCHEMA)).thenReturn(map);
    when(this.map.get(SolrConstants.COPY_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.DYNAMIC_FIELDS)).thenReturn(maps);
    when(this.map.get(SolrConstants.SOLR_FIELDS)).thenReturn(new ArrayList<SimpleOrderedMap>());
    when(this.destinationClient.add(Mockito.anyList())).thenThrow(RuntimeException.class);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    this.solrIndexingRepositoryBean.updateAll(sourceClient, destinationClient);
    verify(this.queryResponse, times(4)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.queryResponse).getResponse();
    verify(this.sourceClient, times(3)).query(solrQueryCaptor.capture());
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.map).get(SolrConstants.SOLR_FIELDS);
    verify(this.map).get(SolrConstants.DYNAMIC_FIELDS);
    verify(this.map).get(SolrConstants.COPY_FIELDS);
    verify(this.namedList).get(SolrConstants.SCHEMA);
    verify(this.destinationClient).commit();
  }

  @Test
  public void copyProductsToL3CollectionEmptyCategoryCodesTest() throws Exception {
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    when(this.itemService.getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productSkuAndProductAndItemsVOMap);
    this.solrIndexingRepositoryBean
        .copyProductsToL3Collection(DEFAULT_STORE_ID, sourceClient, destinationClient, new ArrayList<>(), SolrConstants.ASC, 1000,
            0);
    verify(this.queryResponse, times(6)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.sourceClient, times(2)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.itemService, times(2))
        .getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList());
    Assertions.assertEquals(QUERY, solrQueryCaptor.getValue().getQuery());
    Assertions.assertEquals(IMAGE, listArgumentCaptor.getValue().get(0).getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE));
  }

  @Test
  public void copyProductsToL3CollectionTest() throws Exception {
    List<String> categoryCodes = new ArrayList<>();
    categoryCodes.add(SolrFieldNames.MASTER_CATALOG);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenReturn(0);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    when(this.itemService.getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productSkuAndProductAndItemsVOMap);
    this.solrIndexingRepositoryBean
        .copyProductsToL3Collection(DEFAULT_STORE_ID, sourceClient, destinationClient, categoryCodes, SolrConstants.DESC, 1000,
            0);
    verify(this.queryResponse, times(6)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.sourceClient, times(2)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.itemService, times(2))
        .getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList());
    Assertions.assertEquals(IMAGE, listArgumentCaptor.getValue().get(0).getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE));
    Assertions.assertEquals(MASTER_CATLOG_QUERY, solrQueryCaptor.getValue().getQuery());
  }

  @Test
  public void copyProductsToL3CollectionExceptionTest() throws Exception {
    List<String> categoryCodes = new ArrayList<>();
    categoryCodes.add(SolrFieldNames.MASTER_CATALOG);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getNextCursorMark()).thenReturn("50").thenReturn("50");
    when(this.sourceClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    when(this.queryResponse.getResponse()).thenReturn(namedList);
    when(this.destinationClient.add(Mockito.anyList())).thenReturn(updateResponse);
    when(this.updateResponse.getStatus()).thenThrow(RuntimeException.class);
    when(this.destinationClient.commit()).thenReturn(updateResponse);
    when(this.itemService.getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList()))
        .thenReturn(productSkuAndProductAndItemsVOMap);
    this.solrIndexingRepositoryBean
        .copyProductsToL3Collection(DEFAULT_STORE_ID, sourceClient, destinationClient, categoryCodes, SolrConstants.DESC, 1000,
            0);
    verify(this.queryResponse, times(6)).getResults();
    verify(this.queryResponse, times(4)).getNextCursorMark();
    verify(this.sourceClient, times(2)).query(solrQueryCaptor.capture());
    verify(this.destinationClient).commit();
    verify(this.destinationClient, times(2)).add(listArgumentCaptor.capture());
    verify(this.updateResponse, times(2)).getStatus();
    verify(this.itemService, times(2))
        .getProductAndItemsMap(eq(DEFAULT_STORE_ID), Mockito.anyList());
    Assertions.assertEquals(IMAGE, listArgumentCaptor.getValue().get(0).getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE));
    Assertions.assertEquals(MASTER_CATLOG_QUERY, solrQueryCaptor.getValue().getQuery());
  }

  @Test
  public void updateBatchTest() throws IOException, SolrServerException {
    when(this.destinationClient.add(Mockito.anyList()))
        .thenReturn(updateResponse);
    solrIndexingRepositoryBean
        .updateBatch(destinationClient, Arrays.asList(new SolrInputDocument()));
    verify(this.destinationClient).add(Mockito.anyList());
  }

  @Test
  public void updateBatch_expectExceptionTest() throws IOException, SolrServerException {
    when(this.destinationClient.add(Mockito.anyList()))
        .thenThrow(RuntimeException.class);
    try {
      Assertions.assertThrows(RuntimeException.class, () -> solrIndexingRepositoryBean
          .updateBatch(destinationClient, Arrays.asList(new SolrInputDocument())));
    } finally {
      verify(this.destinationClient).add(Mockito.anyList());
    }
  }
}