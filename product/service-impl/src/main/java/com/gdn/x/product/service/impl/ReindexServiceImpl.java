package com.gdn.x.product.service.impl;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.PristineItemUpdateRepository;
import com.gdn.x.product.dao.api.ProductL3SolrReindexStatusRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.solr.api.SolrIndexingRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.ItemSolrReindexEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
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
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ReindexService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SolrIndexService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ReindexServiceImpl implements ReindexService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ReindexServiceImpl.class);

  @Autowired
  private SolrIndexService solrIndexService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  @Lazy
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ProductSearchHelperService productSearchHelper;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private SolrIndexingRepository solrIndexingRepository;

  @Autowired
  private PristineItemUpdateRepository pristineItemUpdateRepository;

  @Autowired
  private DeferredSolrReindexItemService deferredSolrReindexItemService;

  @Autowired
  private ProductL3SolrReindexStatusRepository productL3SolrReindexStatusRepository;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  @Qualifier("xproductL3Client")
  private CloudSolrClient cloudSolrClientL3;

  @Value("${job.pristineData.update.batch.size}")
  private int pageSize;

  @Value("${product.visibility.switch.enabled}")
  private boolean isPristineSettingEnabled ;

  @Value("${solr.cloud.urls}")
  private String solrCloudUrls;

  @Value("${product.l3.solr.reindex.batch.size}")
  private int productL3ReindexBatchSize;

  @Value("${solr.xproduct.l3.collection}")
  private String productL3SolrCollectionName;

  @Value("${pre.order.config.poQuotaFeatureSwitch}")
  private boolean preOrderQuotaSwitch;

  private static final String COMMA_SEPARATOR = ",";
  private static final String SLASH = "/";

  private void reindex(String requestId, String username, String storeId, List<Product> allProducts,
      boolean clearCache, boolean isProductSkuIndexing) throws Exception {
    ReindexServiceImpl.LOGGER.warn("starting full reindex with total {}", allProducts.size());
    int solrIndexSize = Integer.valueOf(this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE).getValue());
    Map<Boolean, List<Product>> collect =
        allProducts.stream().collect(Collectors.groupingBy(e -> e.getProductCode() == null,
            Collectors.mapping(e -> e, Collectors.toList())));
    allProducts = collect.get(false);
    if (allProducts == null) {
      allProducts = new ArrayList<>();
    } else {
      allProducts.sort(Comparator.comparing(Product::getProductCode));
    }
    List<Product> oldProduct = collect.get(true);
    if (oldProduct != null) {
      allProducts.addAll(oldProduct);
    }
    List<List<Product>> listOfAllProducts = Lists.partition(allProducts, solrIndexSize);
    for (List<Product> products : listOfAllProducts) {
      Set<String> productCodes =
          products.stream().map(Product::getProductCode).collect(Collectors.toSet());
      List<String> productSkus =
          products.stream().map(Product::getProductSku).collect(Collectors.toList());
      try {
        MasterDataDetailWithProductAndItemsResponseVo masterDataResponse =
            this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(storeId,
                username, requestId, productCodes, true);
        this.solrIndexService.indexBulkProductWithMasterData(storeId, productSkus,
            masterDataResponse.getMasterDataProducts(), masterDataResponse.getMasterDataItems(),
            clearCache);
      } catch (Exception e) {
        LOGGER.error("#solrIndexFailed: productSku : {}, productCode : {}", productSkus, productCodes, e);
      }
    }
    if (!isProductSkuIndexing) {
      SystemParameter systemParameter =
          this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_LAST_INDEX_TIME);
      systemParameter.setValue(String.valueOf(new Date().getTime()));
      systemParameter.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
      systemParameter.setUpdatedDate(new Date());
      systemParameterService.update(systemParameter);
    }
  }

  private void publishEventToReindexProductsToSolr(List<Product> products) {
    for (Product product : products) {
      try {
        DeltaReindexToSolrEventModel deltaReindexToSolrEventModel = new DeltaReindexToSolrEventModel();
        deltaReindexToSolrEventModel.setProduct(objectConverterService.convertToProductEventModel(product));
        deltaReindexToSolrEventModel.setProductSku(product.getProductSku());
        deltaReindexToSolrEventModel.setRejected(CommonUtil.isProductRejected(product));
        log.info("publishing reindex product event : {} for product sku : {} ",
            ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, product.getProductSku());
        this.kafkaPublisher.send(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR,
          deltaReindexToSolrEventModel.getProductSku(), deltaReindexToSolrEventModel);
      } catch (Exception e) {
        log.error("error while publishing reindex product event : {} for product sku : {} ",
            ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR, product.getProductSku(), e);
      }
    }
  }

  @Override
  @Async
  public void reindexFull(String requestId, String username, String storeId) throws Exception {
    int solrIndexSize = Integer.parseInt(
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE)
            .getValue());
    Pageable pageable = PageRequest.of(0, solrIndexSize);
    SystemParameter solrLastIndexTime =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    SystemParameter solrItemLastIndexTime =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME);
    solrLastIndexTime.setValue(String.valueOf(new Date().getTime()));
    solrLastIndexTime.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
    solrLastIndexTime.setUpdatedDate(new Date());
    systemParameterService.update(solrLastIndexTime);
    solrItemLastIndexTime.setValue(String.valueOf(new Date().getTime()));
    solrItemLastIndexTime.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
    solrItemLastIndexTime.setUpdatedDate(new Date());
    systemParameterService.update(solrItemLastIndexTime);
    Page<Product> allProducts;
    do {
      allProducts = this.productRepository.findBy(pageable);
      pageable = allProducts.nextPageable();
      this.publishEventToReindexProductsToSolr(allProducts.getContent());
    } while (allProducts.hasNext());
  }

  @Override
  @Async
  public void deltaReindex(String storeId, String requestId, String username, String indexFrom, String indexTill)
      throws Exception {
    SystemParameter systemParameter =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_LAST_INDEX_TIME);
    int solrIndexSize = Integer.parseInt(
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE)
            .getValue());
    Date indexFromDate = StringUtils.isEmpty(indexFrom) ?
        new Date(Long.parseLong(systemParameter.getValue())) :
        new SimpleDateFormat(Constants.DATE_FORMAT).parse(indexFrom);
    Date indexTillDate =
        StringUtils.isEmpty(indexTill) ? new Date() : new SimpleDateFormat(Constants.DATE_FORMAT).parse(indexTill);
    reindexRange(storeId, username, requestId, indexFromDate, indexTillDate, solrIndexSize);
    systemParameter.setValue(String.valueOf(indexTillDate.getTime()));
    systemParameter.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
    systemParameter.setUpdatedDate(new Date());
    systemParameterService.update(systemParameter);
  }

  @Override
  @Async
  public void deltaReindexItemSkus(String storeId) throws Exception {
    SystemParameter solrItemLastIndexTime = this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_ITEM_LAST_INDEX_TIME);
    Date indexTillDate = new Date(Long.parseLong(solrItemLastIndexTime.getValue()));
    int solrIndexSize = Integer.parseInt(
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE)
            .getValue());
    Pageable pageable = PageRequest.of(0, solrIndexSize);
    Page<Item> items;
    solrItemLastIndexTime.setValue(String.valueOf(new Date().getTime()));
    do {
      items = this.itemService.findByStoreIdAndUpdatedDateGreaterThan(storeId, indexTillDate, pageable);
      overrideL4DetailsFromL5(storeId, items);
      pageable = items.nextPageable();
      deltaReindexItemSkus(items.getContent());
    } while (items.hasNext());
    solrItemLastIndexTime.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
    solrItemLastIndexTime.setUpdatedDate(new Date());
    systemParameterService.update(solrItemLastIndexTime);
  }

  private void overrideL4DetailsFromL5(String storeId, Page<Item> items) {
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, item.getItemSku());
      objectConverterService
          .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    }
  }

  @Override
  @Async
  public void deltaReindexItemSkuByPristine(String storeId, Pageable pageable) throws Exception {
    long startTime = Calendar.getInstance().getTimeInMillis();
    int sleepTime = Integer.parseInt(
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SLEEP_TIME_FOR_REINDEX)
            .getValue());
    int reindexBatchSize = Integer.parseInt(this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L4).getValue());
    int solrIndexSize = Integer.parseInt(this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE_PRISTINE_ITEM).getValue());
    Page<PristineDataItemUpdate> pristineData;
    int count = 0;
    try {
      do {
        pristineData = pristineItemUpdateRepository.findByIsUpdatedFalse(pageable);
        LOGGER.info("pristineData left to be updated are : {}", pristineData.getTotalElements());
        if (CollectionUtils.isEmpty(pristineData.getContent())) {
          break;
        }
        Set<String> pristineIds =
            pristineData.getContent().stream().map(PristineDataItemUpdate::getPristineId).collect(Collectors.toSet());
        count = count + pristineIds.size();
        List<Item> items = itemService.getAllItemsByPristineIds(storeId, pristineIds);
        LOGGER.info("Total items to be updated : {}", items.size());
        if (CollectionUtils.isEmpty(items)) {
          pristineItemUpdateRepository.updateIsUpdatedFlag(pristineData.getContent());
          continue;
        }
        List<List<Item>> listOfAllProducts = Lists.partition(items, solrIndexSize);
        for (List<Item> itemList : listOfAllProducts) {
          this.productAndItemSolrIndexerService.updateSolrOnPristineChanges(itemList);
        }
        pristineItemUpdateRepository.updateIsUpdatedFlag(pristineData.getContent());
        Thread.sleep(sleepTime);
      } while (count < reindexBatchSize);
      long endtime = Calendar.getInstance().getTimeInMillis();
      LOGGER.info(
          "Update pristineId successful. Total number of pristine Data updated : {} and time taken in milliseconds: {}",
          count, endtime - startTime);
    } catch (Exception e) {
      LOGGER.error("Failed to update pristineId in solr and failed data at page : {}, {}", count, e);
    }
  }

  private void deltaReindexItemSkus(List<Item> items) throws Exception {
    for (Item item : items) {
      this.productAndItemSolrIndexerService.applyItem(item);
    }
  }

  private void reindexRange(String storeId, String username, String requestId,
      Date indexFromDate, Date indexTillDate, int pageSize) throws Exception {
    LOGGER.info("Indexing started for dateRange {} and {}", indexFromDate, indexTillDate);
    Page<Product> allProducts;
    int page = 0;
    do {
      allProducts = this.productRepository.findByUpdatedDateBetween(indexFromDate, indexTillDate,
          PageRequest.of(page, pageSize));
      boolean continuePublishing = Boolean.parseBoolean(systemParameterService
          .findValueByStoreIdAndVariable(storeId, SystemParameterNames.CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT).getValue());
      if (!continuePublishing) {
        break;
      }
      this.publishEventToReindexProductsToSolr(allProducts.getContent());
      page ++;
    } while (allProducts.hasNext());
    LOGGER.info("Indexing done for dateRange {} and {}", indexFromDate, indexTillDate);
  }

  @Override
  public void reindexFullSimple(String requestId, String username, String storeId)
      throws Exception {
    List<Product> allProducts = this.productRepository.findBy();
    boolean clearCache = false;
    this.reindexSimple(requestId, username, storeId, allProducts, clearCache, false);
  }

  @Override
  public void reindexOldProduct(String requestId, String username, String storeId)
      throws Exception {
    List<Product> allProducts = this.productRepository.findByMarkForDeleteFalseAndProductCodeNull();
    this.reindex(requestId, username, storeId, allProducts, false, true);
  }

  private void reindexSimple(String requestId, String username, String storeId,
      List<Product> allProducts, boolean clearCache, boolean isProductSkuIndexing) throws Exception {
    int solrIndexSize = Integer.parseInt(this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_SIZE).getValue());
    ReindexServiceImpl.LOGGER.warn("starting full reindex with total {}", allProducts.size());
    List<List<Product>> listOfAllProducts = Lists.partition(allProducts, solrIndexSize);
    for (List<Product> products : listOfAllProducts) {
      Set<String> productCodes =
          products.stream().map(Product::getProductCode).collect(Collectors.toSet());
      List<String> productSkus =
          products.stream().map(Product::getProductSku).collect(Collectors.toList());
      try {
        MasterDataDetailWithProductAndItemsResponseVo masterDataResponse =
            this.productSearchHelper.getMasterDataProductAndMasterDataItemForReindex(storeId,
                username, requestId, productCodes, false);
        this.solrIndexService.indexBulkProductWithMasterData(storeId, productSkus,
            masterDataResponse.getMasterDataProducts(), masterDataResponse.getMasterDataItems(),
            clearCache);
      } catch (Exception e) {
        ReindexServiceImpl.LOGGER.error("#solrIndexFailed:productSku:{},productCode:{}, cause:{}",
            productSkus, productCodes, e.getMessage());
      }
    }
    if (!isProductSkuIndexing) {
      SystemParameter systemParameter =
          this.systemParameterService.findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_LAST_INDEX_TIME);
      systemParameter.setValue(String.valueOf(new Date()));
      systemParameter.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
      systemParameter.setUpdatedDate(new Date());
      systemParameterService.update(systemParameter);
    }
  }

  @Override
  @Async
  public void reindexSolrAndClearCacheByProductSkus(String requestId, String username,
      String storeId, List<String> productSku) throws Exception {
    List<Product> allProducts =
        this.productRepository.findProductCodeByStoreIdAndProductSkuIn(storeId, productSku);
    boolean clearCache = true;
    this.reindex(requestId, username, storeId, allProducts, clearCache, true);
  }

  @Override
  public void reindexSolrAndClearCacheByProductSkusSimple(String requestId, String username,
      String storeId, List<String> productSku) throws Exception {
    List<Product> allProducts =
        this.productRepository.findProductCodeByStoreIdAndProductSkuIn(storeId, productSku);
    boolean clearCache = true;
    this.reindexSimple(requestId, username, storeId, allProducts, clearCache, true);
  }

  @Override
  @Async
  public void updateAll(String sourceCollection, String destinationCollection) throws Exception {
    try {
      CloudSolrClient sourceSolrClient =
          new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(COMMA_SEPARATOR))).build();
      sourceSolrClient.setDefaultCollection(sourceCollection);
      CloudSolrClient destinationSolrClient =
          new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(COMMA_SEPARATOR))).build();
      destinationSolrClient.setDefaultCollection(destinationCollection);
      solrIndexingRepository.updateAll(sourceSolrClient, destinationSolrClient);
    } catch (Exception e) {
      LOGGER.error("Exception caught while updating documents to solr ", e);
    }
  }

  @Override
  @Async
  public void copyProductsToL3Collection(String sourceCollection, String destinationCollection,
      List<String> categoryCodes, String sortOrder, String storeId) throws Exception {
    try {
      CloudSolrClient sourceSolrClient =
          new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(COMMA_SEPARATOR))).build();
      sourceSolrClient.setDefaultCollection(sourceCollection);
      CloudSolrClient destinationSolrClient =
          new CloudSolrClient.Builder().withZkHost(Arrays.asList(solrCloudUrls.split(COMMA_SEPARATOR))).build();
      destinationSolrClient.setDefaultCollection(destinationCollection);
      SystemParameter solrReindexBatchSize = systemParameterService
          .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_REINDEX_BATCH_SIZE_L3);
      int batchSize = Objects.isNull(solrReindexBatchSize) ? 5000 : Integer.valueOf(solrReindexBatchSize.getValue());
      solrIndexingRepository
          .copyProductsToL3Collection(storeId, sourceSolrClient, destinationSolrClient,
              categoryCodes, sortOrder, batchSize, Integer.parseInt(systemParameterService
                  .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
                      SystemParameterNames.INVENTORY_BATCH_SIZE).getValue()));
    } catch (Exception e) {
      LOGGER.error("Exception caught while updating documents to solr ", e);
    }
  }

  @Override
  @Async
  public void reindexDeferredItemsFirstPage(String storeId) throws Exception {
    SystemParameter deferredItemReindexPageSize = systemParameterService.findValueByStoreIdAndVariable(
        storeId, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    int pageSize = Objects.isNull(deferredItemReindexPageSize) ? 100 : Integer.valueOf(deferredItemReindexPageSize.getValue());
    Pageable pageable = PageRequest.of(0, pageSize);
    Page<DeferredSolrReindexItem> deferredSolrReindexItems;
    deferredSolrReindexItems = deferredSolrReindexItemService.findByStoreId(storeId, pageable);
    List<String> processedItemSkus = new ArrayList<>();
    for (DeferredSolrReindexItem deferredSolrReindexItem : deferredSolrReindexItems) {
      if(processedItemSkus.contains(deferredSolrReindexItem.getItemSku())) {
        deferredSolrReindexItem.setMarkForDelete(true);
        deferredSolrReindexItemService.save(deferredSolrReindexItem);
      } else if(isProcessEnabled(storeId, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX)) {
        Item item =
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, deferredSolrReindexItem.getItemSku());
        ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, item.getItemSku());
        objectConverterService
            .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
        productAndItemSolrIndexerService.applyItem(item);
        deferredSolrReindexItem.setMarkForDelete(true);
        deferredSolrReindexItemService.save(deferredSolrReindexItem);
        processedItemSkus.add(deferredSolrReindexItem.getItemSku());
      }
    }
  }

  @Override
  @Async
  public void reindexDeferredItemsByReindexType(String storeId, String reindexType, String orderBy, String status) {
    SystemParameter deferredItemReindexPageSize = systemParameterService.findValueByStoreIdAndVariable(
        storeId, SystemParameterNames.DEFERRED_SOLR_REINDEX_PAGE_SIZE);
    int pageSize = Objects.isNull(deferredItemReindexPageSize) ? 100 : Integer.valueOf(deferredItemReindexPageSize.getValue());
    Pageable pageable = PageRequest.of(0, pageSize, Sort.by(Sort.Direction.ASC, orderBy));
    List<DeferredSolrReindexItem> deferredSolrReindexItems =
        deferredSolrReindexItemService.findByStoreIdAndReindexTypeAndProductReindexStatus(storeId, reindexType,
            ProductReindexStatus.valueOf(status), pageable);
    if (CollectionUtils.isNotEmpty(deferredSolrReindexItems) && isProcessEnabled(storeId, SystemParameterNames.PROCESS_DEFERRED_SOLR_REINDEX)) {
      int eventSize;
      if (reindexType.equalsIgnoreCase(ReindexType.DELETE_FROM_SOLR.name())) {
        eventSize = Optional.ofNullable(systemParameterService.findValueByStoreIdAndVariable(storeId,
                SystemParameterNames.DELETE_FROM_SOLR_EVENT_SIZE)).map(SystemParameter::getValue).map(Integer::parseInt)
            .orElse(1);
      } else {
        eventSize = Optional.ofNullable(systemParameterService.findValueByStoreIdAndVariable(storeId,
                SystemParameterNames.DEFERRED_SOLR_REINDEX_EVENT_SIZE)).map(SystemParameter::getValue)
            .map(Integer::parseInt).orElse(1);
      }
      List<List<DeferredSolrReindexItem>> subLists = Lists.partition(deferredSolrReindexItems, eventSize);
      for (List<DeferredSolrReindexItem> deferredSolrReindexItemsList : subLists) {
        List<String> itemSkus =
            deferredSolrReindexItemsList.stream().map(DeferredSolrReindexItem::getItemSku).collect(Collectors.toList());
        LOGGER.info("Publishing reindex event of type : {} skus : {} ", reindexType, itemSkus);
        this.kafkaPublisher.send(ProductDomainEventName.L4_SOLR_REINDEX_EVENT_NAME,
          ItemSolrReindexEvent.builder().itemSkus(itemSkus).status(reindexType).build());
      }
    }
  }

  @Override
  @Async
  public void deltaReindexToL3Collection(String storeId, String indexFrom, String indexTill) throws Exception {
    SystemParameter systemParameter = this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SOLR_LAST_INDEX_TIME_L3);
    Date indexFromDate = StringUtils.isEmpty(indexFrom) ?
        new Date(Long.parseLong(systemParameter.getValue())) :
        new SimpleDateFormat(Constants.DATE_FORMAT).parse(indexFrom);
    Date indexTillDate =
        StringUtils.isEmpty(indexTill) ? new Date() : new SimpleDateFormat(Constants.DATE_FORMAT).parse(indexTill);
    List<Product> productList = this.productRepository
        .streamAllByStoreIdAndUpdatedDateBetweenOrderByUpdatedDateAsc(storeId, indexFromDate,
            indexTillDate).collect(Collectors.toList());
    for (Product product : productList) {
      boolean isPreOrder = Objects.nonNull(product.getPreOrder()) &&
          ProductAndItemsUtil.isPreOrderActive(product.getPreOrder().getPreOrderDate());
      boolean productHasStock = inventoryOutbound.isProductInStock(product.getProductSku(), isPreOrder);
      List<Item> itemList = itemService.getItemsByProductSkuFromCacheOrElseDB(storeId, product.getProductSku());
      overrideL4DetailsFromL5(storeId, new PageImpl<>(itemList));
      this.productAndItemSolrIndexerService.reindexProductToL3Collection(product, itemList, productHasStock);
    }
    systemParameter.setValue(String.valueOf(indexTillDate.getTime()));
    systemParameter.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
    systemParameterService.update(systemParameter);
  }

  @Override
  @Async
  public void populateL3CollectionWithNewFields(String storeId, int maxReindexCount,
      String reindexType) throws InterruptedException {
    GdnPreconditions.checkArgument(maxReindexCount > 0, ErrorMessages.REINDEX_COUNT_LIMIT_ZERO);
    ProductReindexStatus productReindexStatusRequest = null;
    if (StringUtils.isNotEmpty(reindexType)) {
      productReindexStatusRequest = ProductReindexStatus.valueOf(reindexType);
      GdnPreconditions
          .checkArgument(!ProductReindexStatus.REINDEX_SUCCESS.equals(productReindexStatusRequest),
              ErrorMessages.REINDEX_SUCCESS_PRODUCT);
    }
    Map<String, Object> reindexControlSwitches = fetchReindexSwitches(storeId);
    if (Boolean.TRUE
        .equals(reindexControlSwitches.get(SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR))) {
      return;
    }
    if ((Integer) reindexControlSwitches
        .get(SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE) > 0) {
      maxReindexCount = (Integer) reindexControlSwitches
          .get(SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE);
    }
    int totalProductReindexedCount = 0;
    List<ProductL3SolrReindexStatus> productL3SolrReindexStatuses = null;
    while (totalProductReindexedCount < maxReindexCount) {
      int productFetchPageSize =
          (Integer) reindexControlSwitches.get(SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE) > 0 ?
              (Integer) reindexControlSwitches.get(SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE) :
              productL3ReindexBatchSize;
      productL3SolrReindexStatuses =
          fetchProductReindexStatusList(storeId, productReindexStatusRequest,
              maxReindexCount, totalProductReindexedCount, productFetchPageSize);
      if (CollectionUtils.isEmpty(productL3SolrReindexStatuses)){
        break;
      }
      publishReindexEvents(productL3SolrReindexStatuses, reindexControlSwitches, reindexType,
          (Integer) reindexControlSwitches
              .get(SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES));
      this.productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(
          productL3SolrReindexStatuses.stream().map(ProductL3SolrReindexStatus::getProductSku)
              .collect(Collectors.toList()), ProductReindexStatus.REINDEX_STARTED);
      Thread.sleep((Integer) reindexControlSwitches
          .get(SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS));
      totalProductReindexedCount += productL3SolrReindexStatuses.size();
    }
  }

  private List<ProductL3SolrReindexStatus> fetchProductReindexStatusList(String storeId,
      ProductReindexStatus productReindexStatusRequest, int maxReindexCount,
      int totalProductReindexedCount, int productFetchPageSize) {
    int queryLimit = productFetchPageSize;
    if (totalProductReindexedCount + productFetchPageSize >= maxReindexCount) {
      queryLimit = maxReindexCount - totalProductReindexedCount;
    }
    if (Objects.nonNull(productReindexStatusRequest)) {
      return this.productL3SolrReindexStatusRepository
          .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(storeId,
              productReindexStatusRequest, queryLimit);
    } else {
      return this.productL3SolrReindexStatusRepository
          .findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(storeId,
              ProductReindexStatus.REINDEX_PENDING, queryLimit);
    }
  }

  private void publishReindexEvents(List<ProductL3SolrReindexStatus> productL3SolrReindexStatuses,
      Map<String, Object> reindexControlSwitches, String reindexType, Integer waitTimeBetweenEvents)
      throws InterruptedException {
    if (CollectionUtils.isNotEmpty(productL3SolrReindexStatuses)) {
      for (List<ProductL3SolrReindexStatus> productL3SolrReindexStatusBatch : Lists
          .partition(productL3SolrReindexStatuses, (Integer) reindexControlSwitches
              .get(SystemParameterNames.REINDEX_L3_EVENT_PAYLOAD_SIZE))) {
        saveAndPublishService.publishProductL3SolrReindexEvent(
            productL3SolrReindexStatusBatch.stream().map(ProductL3SolrReindexStatus::getProductSku)
                .collect(Collectors.toList()), reindexType);
        if (waitTimeBetweenEvents > 0) {
          Thread.sleep(waitTimeBetweenEvents);
        }
      }
    }
  }

  private Map<String, Object> fetchReindexSwitches(String storeId) {
    Map<String, Object> reindexSwitches = new HashMap<>();
    reindexSwitches.put(SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR, Boolean.valueOf(
        systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.DISABLE_REINDEX_JOB_L3_SOLR).getValue()));
    reindexSwitches.put(SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE, Integer
        .valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE).getValue()));
    reindexSwitches.put(SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE, Integer.valueOf(
        systemParameterService
            .findValueByStoreIdAndVariable(storeId, SystemParameterNames.REINDEX_L3_QUERY_PAGE_SIZE)
            .getValue()));
    reindexSwitches.put(SystemParameterNames.REINDEX_L3_EVENT_PAYLOAD_SIZE, Integer.valueOf(
        systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.REINDEX_L3_EVENT_PAYLOAD_SIZE).getValue()));
    reindexSwitches.put(SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES, Integer.valueOf(
        systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES).getValue()));
    reindexSwitches.put(SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS, Integer.valueOf(
        systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS).getValue()));
    return reindexSwitches;
  }

  @Override
  public void reindexNewFieldsToL3Collection(List<String> productSkuList) {
    List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
    try {
      for (String productSku : productSkuList) {
        try {
          List<Item> itemList = itemService
              .getItemsByProductSkuFromCacheOrElseDB(Constants.DEFAULT_STORE_ID, productSku);
          Product product = productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, productSku);
          boolean isPreOrder = preOrderQuotaSwitch && Objects.nonNull(product.getPreOrder()) &&
              ProductAndItemsUtil.isPreOrderActive(product.getPreOrder().getPreOrderDate());
          boolean productHasStock = inventoryOutbound.isProductInStock(productSku, isPreOrder);
          solrInputDocumentList.add(
              CommonUtil.getSolrInputDocumentForNewFieldsL3Solr(productSku, product, itemList, productHasStock));
        } catch (Exception e) {
          LOGGER.error("Error generating solr input document for : {} ", productSku, e);
          productL3SolrReindexStatusRepository
              .updateProductReindexStatusByProductSku(Arrays.asList(productSku),
                  ProductReindexStatus.REINDEX_FAILED);
        }
      }
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        cloudSolrClientL3.add(solrInputDocumentList);
        productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
            ProductReindexStatus.REINDEX_SUCCESS);
      }
    } catch (Exception e) {
      LOGGER.error("Error updating solr for L3 : {} ", productSkuList, e);
      productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
          ProductReindexStatus.REINDEX_FAILED);
    }
  }

  @Override
  public void reindexNewFlagValuesToL3Collection(List<String> productSkuList) {
    List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
    try {
      List<Product> allProducts =
          productRepository.findByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, productSkuList);
      for (Product product : allProducts) {
        solrInputDocumentList.add(CommonUtil.getSolrInputDocumentForL3Solr(product));
      }
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        cloudSolrClientL3.add(solrInputDocumentList);
        productL3SolrReindexStatusRepository
            .updateProductReindexStatusByProductSku(productSkuList, ProductReindexStatus.REINDEX_SUCCESS);
      }
    } catch (Exception e) {
      LOGGER.error("Error updating solr for L3 : {} ", productSkuList, e);
      productL3SolrReindexStatusRepository
          .updateProductReindexStatusByProductSku(productSkuList, ProductReindexStatus.REINDEX_FLAGS_FAILED);
    }
  }

  @Override
  public void updateStatusInL3ReindexCollection(List<String> productSkuList, ProductReindexStatus status) {
    productL3SolrReindexStatusRepository
        .updateProductReindexStatusByProductSku(productSkuList, status);
  }

  @Override
  public void reindexOfflineItems(List<String> itemSkuList) {
    List<DeferredSolrReindexItem> deferredSolrReindexItems =
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID, itemSkuList);
    LOGGER.debug("deferredSolrReindexItems : {} ", deferredSolrReindexItems);
    try {
      if (CollectionUtils.isNotEmpty(deferredSolrReindexItems)) {
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_STARTED, false);
        List<Item> items = itemService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, new HashSet<>(itemSkuList));
        overrideL4DetailsFromL5(Constants.DEFAULT_STORE_ID, new PageImpl<>(items));
        if (isProcessEnabled(Constants.DEFAULT_STORE_ID, SystemParameterNames.PROCESS_ATOMIC_SOLR_REINDEX)) {
          productAndItemSolrIndexerService.reindexOfflineItemAndCncActivatedFlag(Constants.DEFAULT_STORE_ID, items);
        } else {
          productAndItemSolrIndexerService.applyItems(items);
        }
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_SUCCESS, true);
      }
    } catch (Exception e) {
      LOGGER.error("Exception on updating offline items : {} ", itemSkuList, e);
      updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_FAILED, true);
    }
  }

  @Override
  public void reindexItems(List<String> itemSkuList) {
    List<DeferredSolrReindexItem> deferredSolrReindexItems =
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID, itemSkuList);
    try {
      if (CollectionUtils.isNotEmpty(deferredSolrReindexItems)) {
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_STARTED, false);
        List<Item> items = itemService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, new HashSet<>(itemSkuList));
        overrideL4DetailsFromL5(Constants.DEFAULT_STORE_ID, new PageImpl<>(items));
        productAndItemSolrIndexerService.applyItems(items);
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_SUCCESS, true);
      }
    } catch (Exception e) {
      LOGGER.error("Exception on updating items : {} ", itemSkuList, e);
      updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_FAILED, true);
    }
  }

  private void updateStatus(List<DeferredSolrReindexItem> deferredSolrReindexItems,
      ProductReindexStatus reindexStarted, boolean markForDelete) {
    deferredSolrReindexItems.forEach(deferredSolrReindexItem -> {
      deferredSolrReindexItem.setProductReindexStatus(reindexStarted);
      deferredSolrReindexItem.setMarkForDelete(markForDelete);
    });
    deferredSolrReindexItemService.save(deferredSolrReindexItems);
  }

  @Override
  public void reindexPendingL3Collection(List<String> productSkuList) {
    try {
      fetchAndSavePickupPointCodeToProduct(productSkuList, new HashMap<>());
      productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
        ProductReindexStatus.REINDEX_PENDING_L3_SUCCESS);
    } catch (Exception e) {
      LOGGER.error("Error updating solr for L3 : {} ", productSkuList, e);
      productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
        ProductReindexStatus.REINDEX_PENDING_L3_FAILED);
    }
  }

  private void fetchAndSavePickupPointCodeToProduct(List<String> productSkuList,
      Map<String, Product> productSkuAndProductMap) throws Exception {
    List<Product> allProducts = new ArrayList<>();
    List<String> fetchFromDb = new ArrayList<>();
    for (String productSku : productSkuList) {
      if (productSkuAndProductMap.containsKey(productSku)) {
        allProducts.add(productSkuAndProductMap.get(productSku));
      } else {
        fetchFromDb.add(productSku);
      }
    }
    if (CollectionUtils.isNotEmpty(fetchFromDb)) {
      allProducts
          .addAll(this.productRepository.findProductByStoreIdAndProductSkuIn(Constants.DEFAULT_STORE_ID, fetchFromDb, false));
    }
    if (CollectionUtils.isNotEmpty(allProducts)) {
      for (Product product : allProducts) {
        product.setPickupPointCodes(CommonUtil.getDistinctPickupPointCodes(this.itemPickupPointService
            .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
                product.getProductSku())));
      }
      List<Product> updateProductList = saveAndPublishService.saveProducts(allProducts);
      productAndItemSolrIndexerService.applyPendingProductReindex(updateProductList, productSkuList);
    }
  }

  @Override
  public void reindexPendingL3SolrAndDatabase(List<String> productSkuList,
      Map<String, Product> productSkuAndProductMap) {
    try {
      fetchAndSavePickupPointCodeToProduct(productSkuList, productSkuAndProductMap);
      productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
        ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_SUCCESS);
    } catch (Exception e) {
      log.error("Error updating solr for L3 : {}", productSkuList, e);
      productL3SolrReindexStatusRepository.updateProductReindexStatusByProductSku(productSkuList,
        ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB_FAILED);
    }
  }

  @Override
  public void deleteProductAndItemsFromSolr(List<String> productSkus) {
    // For this process we will use itemSku as productSku since we will have only productSku to delete
    List<DeferredSolrReindexItem> deferredSolrReindexItems =
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(Constants.DEFAULT_STORE_ID, productSkus);
    try {
      if (CollectionUtils.isNotEmpty(deferredSolrReindexItems)) {
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_STARTED, false);
        productAndItemSolrIndexerService.deleteProductsFromSolrAfterPostLiveRejection(new HashSet<>(productSkus));
        productSkus.forEach(
            productSku -> productAndItemSolrIndexerService.deleteSolrDocumentByProductSkuInL4Solr(productSku));
        updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_SUCCESS, true);
      }
    } catch (Exception exception) {
      log.error("Error while deleting document from solr {} ", productSkus, exception);
      updateStatus(deferredSolrReindexItems, ProductReindexStatus.REINDEX_FAILED, true);
    }
  }

  private boolean isProcessEnabled(String storeId, String variable) {
    SystemParameter processDeferredSolrReindex =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId, variable);
    return Objects.isNull(processDeferredSolrReindex) ? false :
        Boolean.valueOf(processDeferredSolrReindex.getValue());
  }

}
