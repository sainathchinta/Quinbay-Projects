package com.gdn.x.product.service.impl;

import static com.gdn.x.product.enums.ReindexType.DELETE_FROM_SOLR;
import static com.gdn.x.product.enums.ReindexType.ITEM_REINDEX;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemUpdateRepository;
import com.gdn.x.product.dao.api.ProductL3SolrReindexStatusRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.solr.api.SolrIndexingRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.ItemSolrReindexEvent;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.domain.event.model.SolrAddProductEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PristineDataItemUpdate;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.service.api.DeferredSolrReindexItemService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SolrIndexService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class ReindexServiceImplTest {

  private static final String PRODUCT_SKU_3 = "productSku-3";
  private static final String PRODUCT_SKU_2 = "productSku-2";
  private static final String PRODUCT_SKU_1 = "productSku-1";
  private static final String PRODUCT_CODE_1 = "productCode-1";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String INDEX_FROM = "26/12/2012";
  private static final String INDEX_TILL = "26/12/2012";
  private static final String EPOCH_INDEX_TILL = "1561621933000";
  private static final Pageable PAGEABLE = PageRequest.of(0, 10);
  private static final String PRISTINE_ID = "PRI-00";
  private static final String PRISTINE_ID2 = "PRI-02";
  private static final String ID = "id" ;
  private static final String DEFERRED_SOLR_REINDEX_PAGE_SIZE = "50";
  private static final String DELETE_FROM_SOLR_EVENT__SIZE = "50";
  private static final String PROCESS_DEFERRED_SOLR_REINDEX = "true";
  private static final String ITEM_SKU_1 = "itemSku-1";
  private static final String ITEM_SKU_2 = "itemSku-2";
  private static final String ITEM_SKU_3 = "itemSku-3";
  private static final String ITEM_SKU_4 = "itemSku-4";
  private static final String ITEM_ID_1 = "itemId1";
  private static final String ITEM_ID_2 = "itemId2";
  private static final String ITEM_ID_3 = "itemId3";
  private static final String ITEM_ID_4 = "itemId4";
  private static final String INDEX_FROM_2 = "26/12/2012";
  private static final String INDEX_TILL_2 = "26/12/2014";
  private static final int MAX_REINDEX_COUNT = 10;
  private static final int REINDEX_BATCH_SIZE = 2;
  private static final String PRODUCT_L3_COLLECTION_NAME = "productL3CollectionName";
  public static final String UPDATED_DATE = "updatedDate";
  public static final String STORE_ID1 = "10001";
  public static final String FALSE = "false";

  private static Date DATE_INDEX_FROM;
  private static Date DATE_INDEX_TILL;

  private static final SystemParameter SOLR_REINDEX_SIZE =
      new SystemParameter(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE, "3", "");

  private static final SystemParameter CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT =
      new SystemParameter(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT, "true", "");

  private static final SystemParameter SOLR_REINDEX_SIZE_PRISTINE_ITEM =
      new SystemParameter(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM, "3", "");

  private static final SystemParameter SLEEP_TIME_FOR_REINDEX =
      new SystemParameter(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX, "2000", "");

  private static final SystemParameter SOLR_REINDEX_BATCH_SIZE_L4 =
      new SystemParameter(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4, "3", "");

  private SystemParameter systemParameter;
  private Product product1;
  private Product product2;
  private Product product3;
  private Set<String> productCodes;
  private List<Product> products;
  private List<Item> items;
  private MasterDataDetailWithProductAndItemsResponseVo masterDataValue;
  private List<String> categoryCodes;
  private InventoryStockInfoDTO inventoryStockInfoDTO = new InventoryStockInfoDTO();
  private SystemParameter disableJobSystemParameter;
  private SystemParameter maxReindexCountJobSystemParameter;
  private SystemParameter reindexPageSizeSystemParameter;
  private SystemParameter eventPayloadSizeSystemParameter;
  private SystemParameter threadWaitTimeSystemParameter;
  private SystemParameter iterationWaitTimeSystemParameter;
  private SystemParameter inventorySystemParameter;
  private ItemPickupPoint itemPickupPoint;


  private static final String SOURCE_COLLECTION = "SOURCE_COLLECTION";
  private static final String DESTINATION_COLLECTION = "DESTINATION_COLLECTION";
  private static final String SOLR_COLLECTION_URL = "SOLR_COLLECTION_URL";
  private static final String SOLR_CLOUD_URLS = "solrCloudUrls";
  private static final String SLASH = "/";
  private static final String COMMA = ",";

  @Captor
  private ArgumentCaptor<SolrClient> solrClient;

  @Captor
  private ArgumentCaptor<CloudSolrClient> cloudSolrClient;

  @InjectMocks
  private ReindexServiceImpl reindexServiceImpl;

  @Mock
  private SolrIndexService solrIndexService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductSearchHelperService productSearchHelper;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private PristineItemUpdateRepository pristineItemUpdateRepository;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private SolrIndexingRepository solrIndexingRepository;

  @Mock
  private DeferredSolrReindexItemService deferredSolrReindexItemService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private ProductL3SolrReindexStatusRepository productL3SolrReindexStatusRepository;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private CloudSolrClient cloudSolrClientL3;

  @Mock
  private ProductService productService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Captor
  private ArgumentCaptor<SolrAddProductEvent> solrAddProductEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeltaReindexToSolrEventModel> deltaReindexToSolrEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<SystemParameter> systemParameterArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeferredSolrReindexItem> deferredSolrReindexItemArgumentCaptor;

  private SystemParameter deferredItemReindexPageSize;
  private SystemParameter deferredItemReindexEventSize;
  private SystemParameter processDeferredSolrReindex;
  private SystemParameter deleteFromSolrBatchSize;
  private SystemParameter processAtomicUpdateSolr;
  private PageImpl<DeferredSolrReindexItem> deferredSolrReindexItemPage;
  private List<DeferredSolrReindexItem> deferredSolrReindexItems;
  private Page<PristineDataItemUpdate> pristineDataItemUpdates;
  private PristineDataItemUpdate pristineDataItemUpdate;
  private DeferredSolrReindexItem deferredSolrReindexItem1;
  private DeferredSolrReindexItem deferredSolrReindexItem2;
  private DeferredSolrReindexItem deferredSolrReindexItem3;
  private DeferredSolrReindexItem deferredSolrReindexItem4;
  private DeferredSolrReindexItem deferredSolrReindexItem5;
  private DeferredSolrReindexItem deferredSolrReindexItem6;
  private Item item1;
  private Item item2;
  private Item item3;
  private Item item4;
  private ProductL3SolrReindexStatus productL3SolrReindexStatus =
      ProductL3SolrReindexStatus.builder().productSku(PRODUCT_SKU_1)
          .productReindexStatus(ProductReindexStatus.REINDEX_PENDING).build();
  private ProductL3SolrReindexStatus productL3SolrReindexStatus1 =
      ProductL3SolrReindexStatus.builder().productSku(PRODUCT_SKU_2)
          .productReindexStatus(ProductReindexStatus.REINDEX_PENDING).build();

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    ReflectionTestUtils.setField(reindexServiceImpl,"pageSize",20);
    ReflectionTestUtils.setField(reindexServiceImpl, "isPristineSettingEnabled", true);
    systemParameter = new SystemParameter();
    systemParameter.setValue("0");
    systemParameter.setVariable(SystemParameterNames.SOLR_LAST_INDEX_TIME);
    systemParameter.setDescription(SystemParameterNames.SOLR_LAST_INDEX_TIME);

    DATE_INDEX_FROM = new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM);
    DATE_INDEX_TILL = new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL);

    product1 = new Product(PRODUCT_SKU_1, PRODUCT_CODE_1);
    product2 = new Product(PRODUCT_SKU_2, PRODUCT_CODE_1);
    product3 = new Product(PRODUCT_SKU_3, PRODUCT_CODE_1);
    productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE_1);
    products = Arrays.asList(product1, product2, product3);
    masterDataValue =
        new MasterDataDetailWithProductAndItemsResponseVo();
    HashMap<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    HashMap<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataValue.setMasterDataProducts(masterDataProducts);
    masterDataValue.setMasterDataItems(masterDataItems);
    items = new ArrayList<>();
    Item item = new Item();
    item.setId(ID);
    item.setItemSku(SolrFieldNames.ITEM_SKU);
    items.add(item);
    ReflectionTestUtils.setField(reindexServiceImpl, "solrCloudUrls", SOLR_CLOUD_URLS);
    deferredItemReindexPageSize = new SystemParameter();
    deferredItemReindexPageSize.setVariable(SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    deferredItemReindexPageSize.setValue(DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    deferredItemReindexEventSize = new SystemParameter();
    deferredItemReindexEventSize.setVariable(SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE);
    deferredItemReindexEventSize.setValue("1");
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE)).thenReturn(deferredItemReindexPageSize);
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE)).thenReturn(deferredItemReindexEventSize);
    processDeferredSolrReindex = new SystemParameter();
    processDeferredSolrReindex.setVariable(SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    processDeferredSolrReindex.setValue(PROCESS_DEFERRED_SOLR_REINDEX);
    deleteFromSolrBatchSize = new SystemParameter();
    deleteFromSolrBatchSize.setVariable(SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE);
    deleteFromSolrBatchSize.setValue(DELETE_FROM_SOLR_EVENT__SIZE);
    processAtomicUpdateSolr = new SystemParameter();
    processAtomicUpdateSolr.setVariable(SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX);
    processAtomicUpdateSolr.setValue(PROCESS_DEFERRED_SOLR_REINDEX);
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX)).thenReturn(processDeferredSolrReindex);
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE)).thenReturn(deleteFromSolrBatchSize);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID1,
        SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)).thenReturn(processAtomicUpdateSolr);
    deferredSolrReindexItem1 = new DeferredSolrReindexItem();
    deferredSolrReindexItem1.setItemSku(ITEM_SKU_1);
    deferredSolrReindexItem2 = new DeferredSolrReindexItem();
    deferredSolrReindexItem2.setItemSku(ITEM_SKU_2);
    deferredSolrReindexItem3 = new DeferredSolrReindexItem();
    deferredSolrReindexItem3.setItemSku(ITEM_SKU_3);
    deferredSolrReindexItem4 = new DeferredSolrReindexItem();
    deferredSolrReindexItem4.setItemSku(ITEM_SKU_4);
    deferredSolrReindexItem5 = new DeferredSolrReindexItem();
    deferredSolrReindexItem5.setItemSku(ITEM_SKU_1);
    deferredSolrReindexItem6 = new DeferredSolrReindexItem();
    deferredSolrReindexItem6.setItemSku(ITEM_SKU_2);
    deferredSolrReindexItems= new ArrayList<>();
    deferredSolrReindexItems.add(deferredSolrReindexItem1);
    deferredSolrReindexItems.add(deferredSolrReindexItem2);
    deferredSolrReindexItems.add(deferredSolrReindexItem3);
    deferredSolrReindexItems.add(deferredSolrReindexItem4);
    deferredSolrReindexItemPage = new PageImpl<>(deferredSolrReindexItems);
    item1 = new Item();
    item1.setId(ITEM_ID_1);
    item1.setItemSku(ITEM_SKU_1);
    item2 = new Item();
    item2.setId(ITEM_ID_2);
    item2.setItemSku(ITEM_SKU_2);
    item3 = new Item();
    item3.setId(ITEM_ID_3);
    item3.setItemSku(ITEM_SKU_3);
    item4 = new Item();
    item4.setId(ITEM_ID_4);
    item4.setItemSku(ITEM_SKU_4);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_1)).thenReturn(item1);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2)).thenReturn(item2);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_3)).thenReturn(item3);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_4)).thenReturn(item4);

    pristineDataItemUpdate = PristineDataItemUpdate.builder().pristineId(PRISTINE_ID).isUpdated(false).build();
    PristineDataItemUpdate pristineDataItemUpdate1 =
        PristineDataItemUpdate.builder().pristineId(PRISTINE_ID2).isUpdated(false).build();
    pristineDataItemUpdates = new PageImpl<>(Arrays.asList(pristineDataItemUpdate, pristineDataItemUpdate1));

    ReflectionTestUtils.setField(reindexServiceImpl, "productL3ReindexBatchSize", REINDEX_BATCH_SIZE);
    ReflectionTestUtils.setField(reindexServiceImpl, "productL3SolrCollectionName", PRODUCT_L3_COLLECTION_NAME);

    inventoryStockInfoDTO.setWebProductSku(PRODUCT_SKU_1);
    inventoryStockInfoDTO.setWebTotalAvailableStock(0);

    disableJobSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR,
            String.valueOf(false), StringUtils.EMPTY);
    maxReindexCountJobSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE,
            String.valueOf(5), StringUtils.EMPTY);
    reindexPageSizeSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE,
            String.valueOf(0), StringUtils.EMPTY);
    eventPayloadSizeSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_L3_EVENT_PAYLOAD_SIZE,
            String.valueOf(100), StringUtils.EMPTY);
    threadWaitTimeSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES,
            String.valueOf(10), StringUtils.EMPTY);
    iterationWaitTimeSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS,
            String.valueOf(20), StringUtils.EMPTY);
    inventorySystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.INVENTORY_BATCH_SIZE,
            String.valueOf(10), StringUtils.EMPTY);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR))
        .thenReturn(disableJobSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE))
        .thenReturn(maxReindexCountJobSystemParameter);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE))
        .thenReturn(reindexPageSizeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REINDEX_L3_EVENT_PAYLOAD_SIZE))
        .thenReturn(eventPayloadSizeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES))
        .thenReturn(threadWaitTimeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS))
        .thenReturn(iterationWaitTimeSystemParameter);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
            SystemParameterNames.INVENTORY_BATCH_SIZE)).thenReturn(inventorySystemParameter);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU_1);
  }

  @Test
  public void reindexFullSimpleTest() throws Exception {
    Product product1 = new Product(PRODUCT_SKU_1, PRODUCT_CODE_1);
    Product product2 = new Product(PRODUCT_SKU_2, PRODUCT_CODE_1);
    Product product3 = new Product(PRODUCT_SKU_3, PRODUCT_CODE_1);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE_1);
    List<Product> products = Arrays.asList(product1, product2, product3);
    MasterDataDetailWithProductAndItemsResponseVo masterDataValue =
        new MasterDataDetailWithProductAndItemsResponseVo();
    HashMap<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    HashMap<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataValue.setMasterDataProducts(masterDataProducts);
    masterDataValue.setMasterDataItems(masterDataItems);

    when(this.productRepository.findBy()).thenReturn(products);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE)).thenReturn(SOLR_REINDEX_SIZE);
    when(this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, false)).thenReturn(masterDataValue);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME)).thenReturn(systemParameter);

    this.reindexServiceImpl.reindexFullSimple(REQUEST_ID, USERNAME, STORE_ID);

    verify(this.productRepository).findBy();
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.productSearchHelper).getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, false);
    verify(this.solrIndexService).indexBulkProductWithMasterData(STORE_ID,
        Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3), masterDataProducts,
        masterDataItems, false);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService).update(Mockito.any(SystemParameter.class));
  }

  @Test
  public void reindexFullTest() throws Exception {
    when(this.productRepository.findBy()).thenReturn(products);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE)).thenReturn(SOLR_REINDEX_SIZE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME)).thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME)).thenReturn(systemParameter);
    when(this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, false)).thenReturn(masterDataValue);
    when(this.productRepository.findBy(Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(products));
    when(objectConverterService.convertToProductEventModel(any(Product.class))).thenReturn(new ProductEventModel());
    this.reindexServiceImpl.reindexFull(REQUEST_ID, USERNAME, STORE_ID);

    verify(this.productRepository).findBy(Mockito.any(Pageable.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME);
    verify(this.systemParameterService, times(2)).update(Mockito.any(SystemParameter.class));
    verify(objectConverterService, times(3)).convertToProductEventModel(any(Product.class));
    verify(this.kafkaProducer, times(3)).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      anyString(), any(DeltaReindexToSolrEventModel.class));
  }

  @Test
  public void deltaReindexItemSkusTest() throws Exception {
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.itemService.findByStoreIdAndUpdatedDateGreaterThan(eq(STORE_ID), Mockito.any(), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(items));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME))
        .thenReturn(systemParameter);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE))
        .thenReturn(SOLR_REINDEX_SIZE);
    doNothing().when(this.productAndItemSolrIndexerService).applyItem(items.get(0));
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    this.reindexServiceImpl.deltaReindexItemSkus(STORE_ID);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, items.get(0).getItemSku());
    verify(this.itemService).findByStoreIdAndUpdatedDateGreaterThan(eq(STORE_ID), Mockito.any(), Mockito.any(PageRequest.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME);
    verify(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    verify(this.productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
  }

  @Test
  public void deltaIndexBetweenTest() throws Exception {
    when(this.productRepository.findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(products));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE))
        .thenReturn(SOLR_REINDEX_SIZE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME))
        .thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT)).thenReturn(CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT);
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(objectConverterService.convertToProductEventModel(any(Product.class))).thenReturn(new ProductEventModel());

    reindexServiceImpl.deltaReindex(STORE_ID, REQUEST_ID, USERNAME, INDEX_FROM, INDEX_TILL);

    verify(this.productRepository)
        .findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT);
    verify(this.systemParameterService).update(systemParameterArgumentCaptor.capture());
    verify(objectConverterService, times(3)).convertToProductEventModel(any(Product.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_1), deltaReindexToSolrEventModelArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_2), deltaReindexToSolrEventModelArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_3), deltaReindexToSolrEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU_1, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_SKU_2, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(1).getProductSku());
    Assertions.assertEquals(PRODUCT_SKU_3, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(2).getProductSku());
  }

  @Test
  public void deltaIndexLastReindexTest() throws Exception{
    systemParameter.setValue(EPOCH_INDEX_TILL);
    when(this.productRepository.findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(products));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE))
        .thenReturn(SOLR_REINDEX_SIZE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME))
        .thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT, "true", ""));
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(objectConverterService.convertToProductEventModel(any(Product.class))).thenReturn(new ProductEventModel());

    reindexServiceImpl.deltaReindex(STORE_ID, REQUEST_ID, USERNAME, null, null);

    verify(this.productRepository)
        .findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService).update(systemParameterArgumentCaptor.capture());
    verify(objectConverterService, times(3)).convertToProductEventModel(any(Product.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_1), deltaReindexToSolrEventModelArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_2), deltaReindexToSolrEventModelArgumentCaptor.capture());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      eq(PRODUCT_SKU_3), deltaReindexToSolrEventModelArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_SKU_1, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(0).getProductSku());
    Assertions.assertEquals(PRODUCT_SKU_2, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(1).getProductSku());
    Assertions.assertEquals(PRODUCT_SKU_3, deltaReindexToSolrEventModelArgumentCaptor.getAllValues().get(2).getProductSku());
  }

  @Test
  public void deltaIndexLastReindexStopPublishingTest() throws Exception{
    systemParameter.setValue(EPOCH_INDEX_TILL);
    when(this.productRepository.findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(products));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE))
        .thenReturn(SOLR_REINDEX_SIZE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME))
        .thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT, "false", ""));
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(objectConverterService.convertToProductEventModel(any(Product.class))).thenReturn(new ProductEventModel());

    reindexServiceImpl.deltaReindex(STORE_ID, REQUEST_ID, USERNAME, null, null);

    verify(this.productRepository)
        .findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT);
    verify(this.systemParameterService).update(systemParameterArgumentCaptor.capture());
  }

  @Test
  public void deltaIndexLastReindexExceptionTestTest() throws Exception{
    systemParameter.setValue(EPOCH_INDEX_TILL);
    when(this.productRepository.findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class)))
        .thenReturn(new PageImpl<>(products));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE))
        .thenReturn(SOLR_REINDEX_SIZE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME))
        .thenReturn(systemParameter);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT))
        .thenReturn(new SystemParameter(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT, "true", ""));
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    when(this.productSearchHelper
        .getMasterDataProductAndMasterDataItemForReindex(STORE_ID, USERNAME, REQUEST_ID, productCodes,
            false))
        .thenReturn(masterDataValue);
    doThrow(ApplicationRuntimeException.class).when(objectConverterService).convertToProductEventModel(any(Product.class));

    reindexServiceImpl.deltaReindex(STORE_ID, REQUEST_ID, USERNAME, null, null);

    verify(this.productRepository)
        .findByUpdatedDateBetween(Mockito.any(), Mockito.any(), Mockito.any(PageRequest.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    verify(this.systemParameterService).update(systemParameterArgumentCaptor.capture());
    verify(objectConverterService, times(3)).convertToProductEventModel(any(Product.class));
  }

  @Test
  public void reindexOldProductTest() throws Exception {
    Product product1 = new Product(PRODUCT_SKU_1);
    Product product2 = new Product(PRODUCT_SKU_2);
    Product product3 = new Product(PRODUCT_SKU_3);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(null);
    List<Product> products = Arrays.asList(product1, product2, product3);
    MasterDataDetailWithProductAndItemsResponseVo masterDataValue =
        new MasterDataDetailWithProductAndItemsResponseVo();
    HashMap<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    HashMap<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataValue.setMasterDataProducts(masterDataProducts);
    masterDataValue.setMasterDataItems(masterDataItems);

    when(this.productRepository.findByMarkForDeleteFalseAndProductCodeNull()).thenReturn(products);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE)).thenReturn(SOLR_REINDEX_SIZE);
    when(this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, true)).thenReturn(masterDataValue);

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME)).thenReturn(systemParameter);

    this.reindexServiceImpl.reindexOldProduct(REQUEST_ID, USERNAME, STORE_ID);

    verify(this.productRepository).findByMarkForDeleteFalseAndProductCodeNull();
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.productSearchHelper).getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, true);
    verify(this.solrIndexService).indexBulkProductWithMasterData(STORE_ID,
        Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3), masterDataProducts,
        masterDataItems, false);
  }

  @Test
  public void reindexSolrAndClearCacheByProductSkusSimpleTest() throws Exception {
    Product product1 = new Product(PRODUCT_SKU_1, PRODUCT_CODE_1);
    Product product2 = new Product(PRODUCT_SKU_2, PRODUCT_CODE_1);
    Product product3 = new Product(PRODUCT_SKU_3, PRODUCT_CODE_1);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE_1);
    List<Product> products = Arrays.asList(product1, product2, product3);
    MasterDataDetailWithProductAndItemsResponseVo masterDataValue =
        new MasterDataDetailWithProductAndItemsResponseVo();
    HashMap<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    HashMap<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataValue.setMasterDataProducts(masterDataProducts);
    masterDataValue.setMasterDataItems(masterDataItems);

    List<String> productSkus = Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3);

    when(this.productRepository.findProductCodeByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(products);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE)).thenReturn(SOLR_REINDEX_SIZE);
    when(this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, false)).thenReturn(masterDataValue);
    this.reindexServiceImpl.reindexSolrAndClearCacheByProductSkusSimple(REQUEST_ID, USERNAME,
        STORE_ID, productSkus);

    verify(this.productRepository).findProductCodeByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.productSearchHelper).getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, false);
    verify(this.solrIndexService).indexBulkProductWithMasterData(STORE_ID,
        Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3), masterDataProducts,
        masterDataItems, true);
  }


  @Test
  public void reindexSolrAndClearCacheByProductSkusTest() throws Exception {
    Product product1 = new Product(PRODUCT_SKU_1, PRODUCT_CODE_1);
    Product product2 = new Product(PRODUCT_SKU_2, PRODUCT_CODE_1);
    Product product3 = new Product(PRODUCT_SKU_3, PRODUCT_CODE_1);
    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE_1);
    List<Product> products = Arrays.asList(product1, product2, product3);
    MasterDataDetailWithProductAndItemsResponseVo masterDataValue =
        new MasterDataDetailWithProductAndItemsResponseVo();
    HashMap<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    HashMap<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataValue.setMasterDataProducts(masterDataProducts);
    masterDataValue.setMasterDataItems(masterDataItems);

    List<String> productSkus = Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3);

    when(this.productRepository.findProductCodeByStoreIdAndProductSkuIn(STORE_ID, productSkus))
        .thenReturn(products);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE)).thenReturn(SOLR_REINDEX_SIZE);
    when(this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, true)).thenReturn(masterDataValue);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_LAST_INDEX_TIME)).thenReturn(systemParameter);
    this.reindexServiceImpl.reindexSolrAndClearCacheByProductSkus(REQUEST_ID, USERNAME, STORE_ID,
        productSkus);

    verify(this.productRepository).findProductCodeByStoreIdAndProductSkuIn(STORE_ID, productSkus);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_REINDEX_SIZE);
    verify(this.productSearchHelper).getMasterDataProductAndMasterDataItemForReindex(STORE_ID,
        USERNAME, REQUEST_ID, productCodes, true);
    verify(this.solrIndexService).indexBulkProductWithMasterData(STORE_ID,
        Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2, PRODUCT_SKU_3), masterDataProducts,
        masterDataItems, true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.solrIndexService);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.productSearchHelper);
    verifyNoMoreInteractions(this.kafkaProducer);
    verifyNoMoreInteractions(this.itemRepository);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(solrIndexingRepository);
    verifyNoMoreInteractions(deferredSolrReindexItemService, pristineItemUpdateRepository);
    verifyNoMoreInteractions(productL3SolrReindexStatusRepository);
    verifyNoMoreInteractions(saveAndPublishService);
    verifyNoMoreInteractions(inventoryOutbound);
    verifyNoMoreInteractions(cloudSolrClientL3);
    verifyNoMoreInteractions(itemPickupPointService);
    verifyNoMoreInteractions(objectConverterService );
  }


  @Test
  public void updateAllTest() throws Exception {
    doNothing().when(this.solrIndexingRepository).updateAll(solrClient.capture(), cloudSolrClient.capture());
    this.reindexServiceImpl.updateAll(SOURCE_COLLECTION, DESTINATION_COLLECTION);
    verify(this.solrIndexingRepository).updateAll(solrClient.capture(), cloudSolrClient.capture());
  }

  @Test
  public void updateAllExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.solrIndexingRepository).updateAll(solrClient.capture(), cloudSolrClient.capture());
    this.reindexServiceImpl.updateAll(SOURCE_COLLECTION, DESTINATION_COLLECTION);
    verify(this.solrIndexingRepository).updateAll(solrClient.capture(), cloudSolrClient.capture());
  }

  @Test
  public void copyProductsToL3CollectionTest() throws Exception {
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L3)).thenReturn(deferredItemReindexPageSize);
    doNothing().when(this.solrIndexingRepository)
        .copyProductsToL3Collection(eq(STORE_ID), solrClient.capture(), cloudSolrClient.capture(), eq(categoryCodes),
            eq(SolrConstants.ASC), eq(50), eq(10));
    this.reindexServiceImpl
        .copyProductsToL3Collection(SOURCE_COLLECTION, DESTINATION_COLLECTION, categoryCodes, SolrConstants.ASC,
            STORE_ID);
    verify(this.solrIndexingRepository)
        .copyProductsToL3Collection(eq(STORE_ID), solrClient.capture(), cloudSolrClient.capture(), eq(categoryCodes),
            eq(SolrConstants.ASC), eq(50), eq(10));
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L3);
    verify(systemParameterService).findValueByStoreIdAndVariable(
        Constants.DEFAULT_STORE_ID, SystemParameterNames.INVENTORY_BATCH_SIZE);
  }

  @Test
  public void copyProductsToL3CollectionExceptionTest() throws Exception {
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L3)).thenReturn(deferredItemReindexPageSize);
    doThrow(Exception.class).when(this.solrIndexingRepository)
        .copyProductsToL3Collection(eq(STORE_ID), solrClient.capture(), cloudSolrClient.capture(), eq(categoryCodes),
            eq(SolrConstants.ASC), eq(50), eq(10));
    this.reindexServiceImpl
        .copyProductsToL3Collection(SOURCE_COLLECTION, DESTINATION_COLLECTION, categoryCodes, SolrConstants.ASC,
            STORE_ID);
    verify(this.solrIndexingRepository)
        .copyProductsToL3Collection(eq(STORE_ID), solrClient.capture(), cloudSolrClient.capture(), eq(categoryCodes),
            eq(SolrConstants.ASC), eq(50), eq(10));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L3);
    verify(systemParameterService).findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.INVENTORY_BATCH_SIZE);
  }

  @Test
  public void reindexDeferredItemsTest() throws Exception {
    when(deferredSolrReindexItemService.findByStoreId(
        eq(STORE_ID), any(Pageable.class))).thenReturn(deferredSolrReindexItemPage);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_1)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_2)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_3)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_4)).thenReturn(itemPickupPoint);
    reindexServiceImpl.reindexDeferredItemsFirstPage(STORE_ID);
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    Pageable pageable = PageRequest.of(0, Integer.valueOf(DEFERRED_SOLR_REINDEX_PAGE_SIZE));
    verify(deferredSolrReindexItemService).findByStoreId(STORE_ID, pageable);
    verify(systemParameterService, times(4)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_1);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_3);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_4);
    verify(productAndItemSolrIndexerService).applyItem(item1);
    verify(productAndItemSolrIndexerService).applyItem(item2);
    verify(productAndItemSolrIndexerService).applyItem(item3);
    verify(productAndItemSolrIndexerService).applyItem(item4);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_1);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_2);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_3);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_4);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item2), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item3), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item4), Collections.singletonList(itemPickupPoint));
    verify(deferredSolrReindexItemService, times(4)).save(deferredSolrReindexItemArgumentCaptor.capture());
    Assertions.assertEquals(ITEM_SKU_1, deferredSolrReindexItemArgumentCaptor.getAllValues().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU_2, deferredSolrReindexItemArgumentCaptor.getAllValues().get(1).getItemSku());
    Assertions.assertEquals(ITEM_SKU_3, deferredSolrReindexItemArgumentCaptor.getAllValues().get(2).getItemSku());
    Assertions.assertEquals(ITEM_SKU_4, deferredSolrReindexItemArgumentCaptor.getAllValues().get(3).getItemSku());
  }

  @Test
  public void reindexDeferredItemsDuplicateItemsTest() throws Exception {
    deferredSolrReindexItems.add(deferredSolrReindexItem5);
    deferredSolrReindexItems.add(deferredSolrReindexItem6);
    deferredSolrReindexItemPage = new PageImpl<>(deferredSolrReindexItems);
    when(deferredSolrReindexItemService.findByStoreId(
        eq(STORE_ID), any(Pageable.class))).thenReturn(deferredSolrReindexItemPage);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_1)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_2)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_3)).thenReturn(itemPickupPoint);
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_4)).thenReturn(itemPickupPoint);
    reindexServiceImpl.reindexDeferredItemsFirstPage(STORE_ID);
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    Pageable pageable = PageRequest.of(0, Integer.valueOf(DEFERRED_SOLR_REINDEX_PAGE_SIZE));
    verify(deferredSolrReindexItemService)
        .findByStoreId(STORE_ID, pageable);
    verify(systemParameterService, times(4)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_1);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_3);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_4);
    verify(productAndItemSolrIndexerService).applyItem(item1);
    verify(productAndItemSolrIndexerService).applyItem(item2);
    verify(productAndItemSolrIndexerService).applyItem(item3);
    verify(productAndItemSolrIndexerService).applyItem(item4);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_1);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_2);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_3);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_4);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item2), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item3), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item4), Collections.singletonList(itemPickupPoint));
    verify(deferredSolrReindexItemService, times(6)).save(deferredSolrReindexItemArgumentCaptor.capture());
    Assertions.assertEquals(ITEM_SKU_1, deferredSolrReindexItemArgumentCaptor.getAllValues().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU_2, deferredSolrReindexItemArgumentCaptor.getAllValues().get(1).getItemSku());
    Assertions.assertEquals(ITEM_SKU_3, deferredSolrReindexItemArgumentCaptor.getAllValues().get(2).getItemSku());
    Assertions.assertEquals(ITEM_SKU_4, deferredSolrReindexItemArgumentCaptor.getAllValues().get(3).getItemSku());
    Assertions.assertEquals(ITEM_SKU_1, deferredSolrReindexItemArgumentCaptor.getAllValues().get(4).getItemSku());
    Assertions.assertEquals(ITEM_SKU_2, deferredSolrReindexItemArgumentCaptor.getAllValues().get(5).getItemSku());
  }

  @Test
  public void reindexDeferredItemsByReindexTypeTest() {
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE);
    verify(this.kafkaProducer, times(4)).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
       any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexType_DeleteFromSolrTest() {
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(DELETE_FROM_SOLR.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any())).thenReturn(
        deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, DELETE_FROM_SOLR.getDescription(), UPDATED_DATE,
        ProductReindexStatus.REINDEX_PENDING.name());
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(DELETE_FROM_SOLR.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
        any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexType_DeleteFromSolr_NullSysParamTest() {
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE)).thenReturn(null);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(DELETE_FROM_SOLR.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any())).thenReturn(
        deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, DELETE_FROM_SOLR.getDescription(), UPDATED_DATE,
        ProductReindexStatus.REINDEX_PENDING.name());
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(DELETE_FROM_SOLR.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE);
    verify(this.kafkaProducer, times(4)).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
        any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexTypeBatchSize2Test() {
    deferredItemReindexEventSize.setValue("2");
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE)).thenReturn(deferredItemReindexEventSize);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE);
    verify(this.kafkaProducer, times(2)).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
      Mockito.any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexTypeBatchSizeNullTest() {
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE)).thenReturn(null);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE);
    verify(this.kafkaProducer, times(4)).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
      Mockito.any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexTypeProcessBatchSizeNullNullTest() {
    ItemSolrReindexEvent reindexEvent =
      ItemSolrReindexEvent.builder().itemSkus(Collections.singletonList(ITEM_SKU_1))
        .status(ITEM_REINDEX.getDescription()).build();
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE)).thenReturn(null);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE);
    verify(this.kafkaProducer, times(4)).send(eq(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME),
      Mockito.any(ItemSolrReindexEvent.class));
  }

  @Test
  public void reindexDeferredItemsByReindexTypeProcessDisabledTest() {
    processDeferredSolrReindex.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX)).thenReturn(processDeferredSolrReindex);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
  }

  @Test
  public void reindexDeferredItemsByReindexTypeEmptyTest() {
    processDeferredSolrReindex.setValue(FALSE);
    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX)).thenReturn(processDeferredSolrReindex);
    when(deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING),
        Mockito.any())).thenReturn(deferredSolrReindexItemPage.getContent());
    reindexServiceImpl.reindexDeferredItemsByReindexType(STORE_ID, ITEM_REINDEX.getDescription(),
        UPDATED_DATE, ProductReindexStatus.REINDEX_PENDING.name());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    verify(deferredSolrReindexItemService).findByStoreIdAndReindexTypeAndProductReindexStatus(eq(STORE_ID),
        eq(ITEM_REINDEX.getDescription()), eq(ProductReindexStatus.REINDEX_PENDING), Mockito.any());
    verify(systemParameterService, times(1)).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX);
  }

  @Test
  public void deltaReindexToL3CollectionTest() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Collections.singletonList(item1), true);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Arrays.asList(item1), true);
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void deltaReindexToL3CollectionPreOrderSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", false);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    product.setPreOrder(preOrder);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Collections.singletonList(item1), true);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Arrays.asList(item1), true);
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void deltaReindexToL3CollectionPreOrderNullDateTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    product.setPreOrder(preOrder);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Collections.singletonList(item1), true);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Arrays.asList(item1), true);
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void deltaReindexToL3CollectionPreOrderPastTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    preOrder.setPreOrderDate(calendar.getTime());
    product.setPreOrder(preOrder);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Collections.singletonList(item1), true);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Arrays.asList(item1), true);
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void deltaReindexToL3CollectionPreOrderFutureTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    preOrder.setPreOrderDate(calendar.getTime());
    product.setPreOrder(preOrder);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Collections.singletonList(item1), true);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, true)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.productAndItemSolrIndexerService)
        .reindexProductToL3Collection(product, Arrays.asList(item1), true);
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, true);
  }

  @Test
  public void deltaReindexToL3Collection_emptyInventoryInfoTest() throws Exception {
    Product product = new Product();
    product.setProductCode(PRODUCT_CODE_1);
    product.setProductSku(PRODUCT_SKU_1);
    Stream<Product> PRODUCT_STREAM = Stream.of(product);
    systemParameter.setValue(String.valueOf(new Date().getTime()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3))
        .thenReturn(systemParameter);
    when(this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(PRODUCT_STREAM);
    doNothing().when(this.systemParameterService).update(Mockito.any(SystemParameter.class));
    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item1  .getItemSku()))
        .thenReturn(itemPickupPoint);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    this.reindexServiceImpl.deltaReindexToL3Collection(STORE_ID, INDEX_FROM_2, INDEX_TILL_2);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    verify(this.productRepository).streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(Mockito.eq(STORE_ID),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_FROM_2)),
        Mockito.eq(new SimpleDateFormat(Constants.DATE_FORMAT).parse(INDEX_TILL_2)));
    verify(this.systemParameterService).update(systemParameter);
    verify(itemService).getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU_1);
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item1), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, item1.getItemSku());
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
    verify(productAndItemSolrIndexerService).reindexProductToL3Collection(product, Collections.singletonList(item1), true);
  }

  @Test
  public void deltaReindexItemSkuByPristineTest() throws Exception {
    Set<String> pristineIds = new HashSet<>(Arrays.asList(PRISTINE_ID, PRISTINE_ID2));
    when(pristineItemUpdateRepository.findByIsUpdatedFalse(PAGEABLE)).thenReturn(pristineDataItemUpdates);
    when(itemService.getAllItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(items);
    doNothing().when(productAndItemSolrIndexerService).updateSolrOnPristineChanges(items);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX))
        .thenReturn(SLEEP_TIME_FOR_REINDEX);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4))
        .thenReturn(SOLR_REINDEX_BATCH_SIZE_L4);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM))
        .thenReturn(SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    doNothing().when(pristineItemUpdateRepository).updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
    this.reindexServiceImpl.deltaReindexItemSkuByPristine(STORE_ID, PAGEABLE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    verify(pristineItemUpdateRepository, times(2)).findByIsUpdatedFalse(PAGEABLE);
    verify(itemService, times(2)).getAllItemsByPristineIds(STORE_ID, pristineIds);
    verify(productAndItemSolrIndexerService, times(2)).updateSolrOnPristineChanges(items);
    verify(pristineItemUpdateRepository, times(2)).updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
  }

  @Test
  public void deltaReindexItemSkuByPristineTest_itemEmpty() throws Exception {
    Set<String> pristineIds = new HashSet<>(Arrays.asList(PRISTINE_ID, PRISTINE_ID2));
    when(pristineItemUpdateRepository.findByIsUpdatedFalse(PAGEABLE)).thenReturn(pristineDataItemUpdates);
    when(itemService.getAllItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(Collections.emptyList());
    doNothing().when(productAndItemSolrIndexerService).updateSolrOnPristineChanges(items);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX))
        .thenReturn(SLEEP_TIME_FOR_REINDEX);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4))
        .thenReturn(SOLR_REINDEX_BATCH_SIZE_L4);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM))
        .thenReturn(SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    doNothing().when(pristineItemUpdateRepository).updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
    this.reindexServiceImpl.deltaReindexItemSkuByPristine(STORE_ID, PAGEABLE);
    verify(pristineItemUpdateRepository, times(2)).findByIsUpdatedFalse(PAGEABLE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    verify(itemService, times(2)).getAllItemsByPristineIds(STORE_ID, pristineIds);
    verify(pristineItemUpdateRepository, times(2)).updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
  }

  @Test
  public void deltaReindexItemSkuByPristineTest_pristineEmpty() throws Exception {
    when(pristineItemUpdateRepository.findByIsUpdatedFalse(PAGEABLE)).thenReturn(new PageImpl<>(new ArrayList<>()));
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX))
        .thenReturn(SLEEP_TIME_FOR_REINDEX);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4))
        .thenReturn(SOLR_REINDEX_BATCH_SIZE_L4);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM))
        .thenReturn(SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    this.reindexServiceImpl.deltaReindexItemSkuByPristine(STORE_ID, PAGEABLE);
    verify(pristineItemUpdateRepository).findByIsUpdatedFalse(PAGEABLE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM);
  }

  @Test
  public void deltaReindexItemSkuByPristineExceptionTest() throws Exception {
    Set<String> pristineIds = new HashSet<>(Arrays.asList(PRISTINE_ID, PRISTINE_ID2));
    when(pristineItemUpdateRepository.findByIsUpdatedFalse(PAGEABLE)).thenReturn(pristineDataItemUpdates);
    when(itemService.getAllItemsByPristineIds(STORE_ID, pristineIds)).thenReturn(items);
    doNothing().when(productAndItemSolrIndexerService).updateSolrOnPristineChanges(items);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX))
        .thenReturn(SLEEP_TIME_FOR_REINDEX);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4))
        .thenReturn(SOLR_REINDEX_BATCH_SIZE_L4);
    when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM))
        .thenReturn(SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    doThrow(ApplicationRuntimeException.class).when(pristineItemUpdateRepository)
        .updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
    this.reindexServiceImpl.deltaReindexItemSkuByPristine(STORE_ID, PAGEABLE);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SLEEP_TIME_FOR_REINDEX);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM);
    verify(pristineItemUpdateRepository).findByIsUpdatedFalse(PAGEABLE);
    verify(itemService).getAllItemsByPristineIds(STORE_ID, pristineIds);
    verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(items);
    verify(pristineItemUpdateRepository).updateIsUpdatedFlag(pristineDataItemUpdates.getContent());
  }

  @Test
  public void populateL3CollectionWithNewFieldsTest() throws Exception {
    Mockito.when(this.productL3SolrReindexStatusRepository
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, REINDEX_BATCH_SIZE))
        .thenReturn(Arrays.asList(productL3SolrReindexStatus)).thenReturn(new ArrayList<>());
    reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT, StringUtils.EMPTY);
    Mockito.verify(this.productL3SolrReindexStatusRepository, times(2))
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, REINDEX_BATCH_SIZE);
    Mockito.verify(this.saveAndPublishService)
        .publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU_1), StringUtils.EMPTY);
    Mockito.verify(this.systemParameterService, times(6))
        .findValueByStoreIdAndVariable(eq(STORE_ID), Mockito.anyString());
    Mockito.verify(this.productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Mockito.anyList(),
            eq(ProductReindexStatus.REINDEX_STARTED));
  }

  @Test
  public void populateL3CollectionWithNewFields_maxReindexCountFailedTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, 0,
        StringUtils.EMPTY));
  }

  @Test
  public void populateL3CollectionWithNewFields_jobDisabledTest() throws Exception {
    disableJobSystemParameter.setValue(String.valueOf(true));
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR))
        .thenReturn(disableJobSystemParameter);
    reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT,
        StringUtils.EMPTY);
    Mockito.verify(this.systemParameterService, times(6))
        .findValueByStoreIdAndVariable(eq(STORE_ID), Mockito.anyString());
  }

  @Test
  public void populateL3CollectionWithNewFields_withoutParameterOverrideTest() throws Exception {
    maxReindexCountJobSystemParameter.setValue(String.valueOf(0));
    reindexPageSizeSystemParameter.setValue(String.valueOf(REINDEX_BATCH_SIZE));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE))
        .thenReturn(maxReindexCountJobSystemParameter);
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE))
        .thenReturn(reindexPageSizeSystemParameter);
    Mockito.when(this.productL3SolrReindexStatusRepository
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, REINDEX_BATCH_SIZE))
        .thenReturn(Arrays.asList(productL3SolrReindexStatus)).thenReturn(new ArrayList<>());
    reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT, StringUtils.EMPTY);
    Mockito.verify(this.productL3SolrReindexStatusRepository, times(2))
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, REINDEX_BATCH_SIZE);
    Mockito.verify(this.saveAndPublishService)
        .publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU_1), StringUtils.EMPTY);
    Mockito.verify(this.systemParameterService, times(6))
        .findValueByStoreIdAndVariable(eq(STORE_ID), Mockito.anyString());
    Mockito.verify(this.productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Mockito.anyList(),
            eq(ProductReindexStatus.REINDEX_STARTED));
  }

  @Test
  public void populateL3CollectionWithNewFields_reindexStatusTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT,
        ProductReindexStatus.REINDEX_SUCCESS.name()));
  }

  @Test
  public void populateL3CollectionWithNewFields_withStatusInputTest() throws Exception {
    Mockito.when(this.productL3SolrReindexStatusRepository
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_FAILED, REINDEX_BATCH_SIZE))
        .thenReturn(Arrays.asList(productL3SolrReindexStatus)).thenReturn(new ArrayList<>());
    reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT,
        ProductReindexStatus.REINDEX_FAILED.name());
    Mockito.verify(this.productL3SolrReindexStatusRepository, times(2))
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_FAILED, REINDEX_BATCH_SIZE);
    Mockito.verify(this.saveAndPublishService)
        .publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_FAILED.name());
    Mockito.verify(this.systemParameterService, times(6))
        .findValueByStoreIdAndVariable(eq(STORE_ID), Mockito.anyString());
    Mockito.verify(this.productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Mockito.anyList(),
            eq(ProductReindexStatus.REINDEX_STARTED));
  }

  @Test
  public void populateL3CollectionWithNewFields_maxReindexCountReachedTest() throws Exception {
    Mockito.when(this.productL3SolrReindexStatusRepository
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, REINDEX_BATCH_SIZE))
        .thenReturn(Arrays.asList(productL3SolrReindexStatus, productL3SolrReindexStatus1));
    Mockito.when(this.productL3SolrReindexStatusRepository
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(STORE_ID,
            ProductReindexStatus.REINDEX_PENDING, 1))
        .thenReturn(Arrays.asList(productL3SolrReindexStatus));
    reindexServiceImpl.populateL3CollectionWithNewFields(STORE_ID, MAX_REINDEX_COUNT,
        StringUtils.EMPTY);
    Mockito.verify(this.productL3SolrReindexStatusRepository, times(3))
        .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(eq(STORE_ID),
            eq(ProductReindexStatus.REINDEX_PENDING), Mockito.anyInt());
    Mockito.verify(this.saveAndPublishService, times(2))
        .publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU_1, PRODUCT_SKU_2), StringUtils.EMPTY);
    Mockito.verify(this.saveAndPublishService)
        .publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU_1), StringUtils.EMPTY);
    Mockito.verify(this.systemParameterService, times(6))
        .findValueByStoreIdAndVariable(eq(STORE_ID), Mockito.anyString());
    Mockito.verify(this.productL3SolrReindexStatusRepository, times(3))
        .updateProductReindexStatusByProductSku(Mockito.anyList(),
            eq(ProductReindexStatus.REINDEX_STARTED));
  }

  @Test
  public void reindexNewFieldsToL3CollectionTest() throws Exception {
    systemParameter.setValue(String.valueOf(10));
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFieldsToL3CollectionPreOrderSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", false);
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    product1.setPreOrder(preOrder);
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFieldsToL3CollectionPreOrderNullPoObjTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFieldsToL3CollectionPreOrderNullTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    product1.setPreOrder(preOrder);
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFieldsToL3CollectionPreOrderPastTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, -1);
    preOrder.setPreOrderDate(calendar.getTime());
    product1.setPreOrder(preOrder);
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFieldsToL3CollectionPreOrderFutureTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    systemParameter.setValue(String.valueOf(10));
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(null);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DAY_OF_MONTH, 1);
    preOrder.setPreOrderDate(calendar.getTime());
    product1.setPreOrder(preOrder);
    Mockito.when(this.itemService.getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, true)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, true);
  }

  @Test
  public void reindexNewFieldsToL3Collection_exceptionOnSolrInputDocumentTest() throws Exception {
    ReflectionTestUtils.setField(reindexServiceImpl, "preOrderQuotaSwitch", true);
    item1.setPrice(null);
    Mockito.when(this.itemService
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
            ProductReindexStatus.REINDEX_FAILED);
  }

  @Test
  public void reindexNewFieldsToL3Collection_exceptionToUpdateTest() throws Exception {
    Mockito.when(this.itemService
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Arrays.asList(item1));
    doThrow(ApplicationRuntimeException.class).when(this.cloudSolrClientL3)
        .add(Mockito.anyList());
    Mockito.when(productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(product1);
    when(inventoryOutbound.isProductInStock(PRODUCT_SKU_1, false)).thenReturn(true);
    reindexServiceImpl.reindexNewFieldsToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.itemService)
        .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
            ProductReindexStatus.REINDEX_FAILED);
    verify(productService).findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1);
    verify(inventoryOutbound).isProductInStock(PRODUCT_SKU_1, false);
  }

  @Test
  public void reindexNewFlagValuesToL3CollectionTest() throws Exception {
    Mockito.when(
        this.productRepository.findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1)))
        .thenReturn(products);
    reindexServiceImpl.reindexNewFlagValuesToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.productRepository)
        .findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1), ProductReindexStatus.REINDEX_SUCCESS);
  }

  @Test
  public void reindexNewFlagValuesToL3Collection_EmptyTest() throws Exception {
    Mockito.when(
        this.productRepository.findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1)))
        .thenReturn(Arrays.asList());
    reindexServiceImpl.reindexNewFlagValuesToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.productRepository)
        .findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1));
  }

  @Test
  public void reindexNewFlagValuesToL3Collection_exceptionToUpdateTest() throws Exception {
    Mockito.when(
        this.productRepository.findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1)))
        .thenReturn(products);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.cloudSolrClientL3).add(Mockito.anyList());
    reindexServiceImpl.reindexNewFlagValuesToL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.cloudSolrClientL3).add(Mockito.anyList());
    Mockito.verify(this.productRepository)
        .findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
            ProductReindexStatus.REINDEX_FLAGS_FAILED);
  }

  @Test
  public void updateStatusInL3ReindexCollection() {
    reindexServiceImpl.updateStatusInL3ReindexCollection(Arrays.asList(PRODUCT_SKU_1),
        ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS);
    Mockito.verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
            ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS);
  }

  @Test
  public void reindexOfflineItems() throws Exception {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(deferredSolrReindexItems);
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    when(itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    reindexServiceImpl.reindexOfflineItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(Mockito.anyString(), eq(SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX));
    Mockito.verify(productAndItemSolrIndexerService).reindexOfflineItemAndCncActivatedFlag(Mockito.anyString(), Mockito.anyList());
    verify(itemPickupPointService).findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void reindexOfflineItemsFullUpdate() throws Exception {
    processAtomicUpdateSolr.setValue(FALSE);
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(deferredSolrReindexItems);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID1,
        SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)).thenReturn(processAtomicUpdateSolr);
    when(itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    reindexServiceImpl.reindexOfflineItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(Mockito.anyString(), eq(SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX));
    Mockito.verify(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void reindexOfflineItemsFullUpdateEmptyList() throws Exception {
    processAtomicUpdateSolr.setValue(FALSE);
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID1,
        SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)).thenReturn(processAtomicUpdateSolr);
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    reindexServiceImpl.reindexOfflineItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void reindexOfflineItemsFullUpdateExceptionTest() throws Exception {
    processAtomicUpdateSolr.setValue(FALSE);
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(deferredSolrReindexItems);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID1,
        SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)).thenReturn(processAtomicUpdateSolr);
    when(itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    reindexServiceImpl.reindexOfflineItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(Mockito.anyString(), eq(SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX));
    Mockito.verify(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void reindexItemsFullUpdateExceptionTest() throws Exception {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(deferredSolrReindexItems);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID1,
        SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)).thenReturn(processAtomicUpdateSolr);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    when(itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    reindexServiceImpl.reindexItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void reindexItemsFullUpdate() throws Exception {
    Mockito.when(
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(deferredSolrReindexItems);
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet()))
        .thenReturn(items);
    when(itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku()))
        .thenReturn(itemPickupPoint);
    reindexServiceImpl.reindexItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService)
        .findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItems(Mockito.anyList());
    verify(itemPickupPointService).findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, items.get(0).getItemSku());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(items, Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void reindexItemsFullUpdateEmptyList() {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet())).thenReturn(items);
    reindexServiceImpl.reindexItems(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void reindexPendingL3CollectionTest() throws Exception {
    List<Product> productList = Collections.singletonList(product1);
    when(this.productRepository.findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(PRODUCT_SKU_1), false)).thenReturn(productList);
    Mockito.when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.saveAndPublishService.saveProducts(productList))
      .thenReturn(productList);
    reindexServiceImpl.reindexPendingL3Collection(Arrays.asList(PRODUCT_SKU_1));
    verify(this.productRepository).findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
      Arrays.asList(PRODUCT_SKU_1), false);
    Mockito.verify(this.itemPickupPointService)
      .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU_1);
    Mockito.verify(this.saveAndPublishService).saveProducts(productList);
    verify(this.productAndItemSolrIndexerService)
      .applyPendingProductReindex(productList, Arrays.asList(PRODUCT_SKU_1));
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_SUCCESS);
  }

  @Test
  public void reindexPendingL3Collection_EmptyTest() {
    Mockito.when(this.productRepository
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU_1), false)).thenReturn(Arrays.asList());
    reindexServiceImpl.reindexPendingL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU_1), false);
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_SUCCESS);
  }

  @Test
  public void reindexPendingL3Collection_exceptionToUpdateTest() throws Exception {
    Mockito.when(this.productRepository
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU_1), false)).thenReturn(products);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU_1), false);
    reindexServiceImpl.reindexPendingL3Collection(Arrays.asList(PRODUCT_SKU_1));
    Mockito.verify(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU_1), false);
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Arrays.asList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_FAILED);
  }

  @Test
  public void reindexPendingL3SolrAndDatabaseTest() throws Exception {
    List<Product> productList = Collections.singletonList(product1);
    Mockito.when(this.productRepository
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false)).thenReturn(productList);
    Mockito.when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.saveAndPublishService.saveProducts(productList))
      .thenReturn(productList);
    reindexServiceImpl.reindexPendingL3SolrAndDatabase(Collections.singletonList(PRODUCT_SKU_1), new HashMap<>());
    Mockito.verify(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false);
    Mockito.verify(this.itemPickupPointService)
      .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU_1);
    Mockito.verify(this.saveAndPublishService).saveProducts(productList);
    verify(this.productAndItemSolrIndexerService)
      .applyPendingProductReindex(productList, Collections.singletonList(PRODUCT_SKU_1));
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Collections.singletonList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_SUCCESS);
  }

  @Test
  public void reindexPendingL3SolrAndDatabase_emptyProductListTest() throws Exception {
    Mockito.when(this.productRepository
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false)).thenReturn(Collections.emptyList());
    reindexServiceImpl.reindexPendingL3SolrAndDatabase(Collections.singletonList(PRODUCT_SKU_1), new HashMap<>());
    Mockito.verify(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false);
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Collections.singletonList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_SUCCESS);
  }

  @Test
  public void reindexPendingL3SolrAndDatabase_exceptionTest() {
    Mockito.when(this.productRepository
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false)).thenThrow(ApplicationRuntimeException.class);
    reindexServiceImpl.reindexPendingL3SolrAndDatabase(Collections.singletonList(PRODUCT_SKU_1), new HashMap<>());
    Mockito.verify(this.productRepository)
      .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
        Collections.singletonList(PRODUCT_SKU_1), false);
    verify(productL3SolrReindexStatusRepository)
      .updateProductReindexStatusByProductSku(Collections.singletonList(PRODUCT_SKU_1),
        ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_FAILED);
  }

  @Test
  public void reindexPendingL3SolrAndDatabaseMapNotEmptyTest() throws Exception {
    List<Product> productList = Collections.singletonList(product1);
    Mockito.when(this.productRepository
        .findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID,
            Collections.singletonList(PRODUCT_SKU_1), false)).thenReturn(productList);
    Mockito.when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, PRODUCT_SKU_1))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.saveAndPublishService.saveProducts(productList))
        .thenReturn(productList);
    Map<String, Product> productSkuAndProduct = new HashMap<>();
    productSkuAndProduct.putIfAbsent(product1.getProductSku(), product1);
    reindexServiceImpl.reindexPendingL3SolrAndDatabase(Collections.singletonList(PRODUCT_SKU_1), productSkuAndProduct);
    Mockito.verify(this.itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            PRODUCT_SKU_1);
    Mockito.verify(this.saveAndPublishService).saveProducts(productList);
    verify(this.productAndItemSolrIndexerService)
        .applyPendingProductReindex(productList, Collections.singletonList(PRODUCT_SKU_1));
    verify(productL3SolrReindexStatusRepository)
        .updateProductReindexStatusByProductSku(Collections.singletonList(PRODUCT_SKU_1),
            ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_SUCCESS);
  }

  @Test
  public void deleteProductAndItemsFromSolrTest() {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4))).thenReturn(deferredSolrReindexItems);
    reindexServiceImpl.deleteProductAndItemsFromSolr(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Set.of(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(productAndItemSolrIndexerService).deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_1);
    Mockito.verify(productAndItemSolrIndexerService).deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_2);
    Mockito.verify(productAndItemSolrIndexerService).deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_3);
    Mockito.verify(productAndItemSolrIndexerService).deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_4);
    Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
  }

  @Test
  public void deleteProductAndItemsFromSolrWithEmptyResultTest() {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4))).thenReturn(new ArrayList<>());
    reindexServiceImpl.deleteProductAndItemsFromSolr(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
  }

  @Test
  public void deleteProductAndItemsFromSolrExceptionTest() {
    Mockito.when(deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4))).thenReturn(deferredSolrReindexItems);
    Mockito.doThrow(new ApplicationRuntimeException()).when(productAndItemSolrIndexerService)
        .deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_1);
    try {
      reindexServiceImpl.deleteProductAndItemsFromSolr(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
    } finally {
      Mockito.verify(deferredSolrReindexItemService).findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID,
          Arrays.asList(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
      Mockito.verify(productAndItemSolrIndexerService)
          .deleteProductsFromSolrAfterPostLiveRejection(Set.of(ITEM_SKU_1, ITEM_SKU_2, ITEM_SKU_3, ITEM_SKU_4));
      Mockito.verify(productAndItemSolrIndexerService).deleteSolrDocumentByProductSkuInL4Solr(ITEM_SKU_1);
      Mockito.verify(deferredSolrReindexItemService, times(2)).save(Mockito.anyList());
    }
  }
}
