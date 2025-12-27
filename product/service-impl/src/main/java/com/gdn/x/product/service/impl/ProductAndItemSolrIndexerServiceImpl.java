package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static java.util.stream.Collectors.toList;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductSolrConstructorService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

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
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
public class ProductAndItemSolrIndexerServiceImpl implements ProductAndItemSolrIndexerService {

  private static final String ITEM_MUST_NOT_BE_EMPTY = "item must not be empty";
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductAndItemSolrIndexerServiceImpl.class);

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  @Lazy
  private ProductAndItemSolrRepository productItemSolrRepository;

  @Autowired
  private ProductSolrRepository productSolrRepository;

  @Autowired
  private ProductAndItemSolrConstructorService productAndItemConstructorService;

  @Autowired
  private ProductSolrConstructorService productSolrConstructorService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectConverterService objectConverter;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  @Qualifier("xproductClient")
  private CloudSolrClient cloudSolrClient;

  @Autowired
  @Qualifier("xproductL3Client")
  private CloudSolrClient cloudSolrClientL3;

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Value("${event.based.solr.update.enabled}")
  private boolean eventBasedSolrUpdateEnable;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${auto.heal.oos.products.with.stock}")
  private boolean autoHealOosProductsWithStock;

  @Value("${populate.label.for.upcoming.promo}")
  private boolean populateLabelForUpcomingPromo;

  @Value("${populate.label.for.pwp.promo}")
  private boolean populateLabelForPwpPromo;

  @Value("${pre.order.config.poQuotaFeatureSwitch}")
  private boolean preOrderQuotaSwitch;

  @Value("${eligible.seller.list.l4.reindex}")
  private Set<String> eligibleSellersForL4Reindex;

  @Override
  public void applyItem(Item item) throws Exception {
    try {
      ProductAndItemSolr productItemSolr = getProductAndItemSolr(item);
      cloudSolrClient.add(CommonUtil.toSolrInputDocument(productItemSolr));
      ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("end indexing item :{}", item.getItemSku());
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error("{}#applyItemSolrError:{}",
          item.getItemSku(), e.getMessage(), e);
      this.saveForRetry(item.getStoreId(), item.getItemSku(), e.getMessage());
      throw e;
    }
  }

  private ProductAndItemSolr getProductAndItemSolr(Item item) {
    ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("start indexing item :{}",
        item.getItemSku());
    ProductAndItemSolr productItemSolr =
        this.productItemSolrRepository.findOne(item.getItemSku(), item.getMerchantCode());
    if (productItemSolr != null) {
      this.productAndItemConstructorService.constructItem(productItemSolr, item, true);
    } else {
      Product product = null;
      try {
        product =
            this.productService.getProductDeletedOrUndeleted(item.getStoreId(),
                item.getProductSku());
      } catch (Exception e) {
        ProductAndItemSolrIndexerServiceImpl.LOGGER.warn(
            "failed to get product data from database {} ", item, e);
      }
      productItemSolr = this.createNewSolrObject(product, item);
    }
    return productItemSolr;
  }

  @Override
  public void applyItems(List<Item> items) throws Exception {
    try {
      List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
      for (Item item : items) {
        ProductAndItemSolr productAndItemSolr = getProductAndItemSolr(item);
        solrInputDocuments.add(CommonUtil.toSolrInputDocument(productAndItemSolr));
      }
      if (CollectionUtils.isNotEmpty(solrInputDocuments)) {
        cloudSolrClient.add(solrInputDocuments);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while reindexing items : {} ",
          items.stream().map(Item::getItemSku).collect(toList()), e);
      throw e;
    }
  }

  @Override
  public void updateInSolrByProductCode(String storeId, String productCode) {
    List<Product> products = this.productService.findByStoreIdAndProductCode(storeId, productCode);
    for (Product product : products) {
      List<Item> items = this.itemService.findItemsByStoreIdAndProductSku(storeId, product.getProductSku());
      ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
      productAndItemEventModel.setProductSku(product.getProductSku());
      productAndItemEventModel.setProduct(objectConverter.convertToProductEventModel(product));
      productAndItemEventModel.setItems(objectConverter.convertToListItemEventModel(items));
      productAndItemEventModel.setMerchantCode(product.getMerchantCode());
      LOGGER.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
          productAndItemEventModel);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel.getProductSku(), productAndItemEventModel);
    }
  }

  @Async
  @Override
  public void applyMasterDataChanges(ProductDomainEventModel productDomainEventModel,
      Map<String, Double> productAndTotalScoreMap) throws Exception {
    try {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .debug("start indexing master data changes :{}", productDomainEventModel.getProductCode());
      List<ProductAndItemSolr> solrProductAndItems =
          this.productItemSolrRepository.findByProductCode(productDomainEventModel.getProductCode());
      List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(solrProductAndItems)) {
        for (ProductAndItemSolr solrProductAndItem : solrProductAndItems) {
          Map<String, FieldValueObject> fieldValueObjectMap = this.productAndItemConstructorService
              .constructByMasterDataChangeModel(solrProductAndItem, productDomainEventModel, productAndTotalScoreMap);
          if (Objects.nonNull(fieldValueObjectMap)) {
            LOGGER.info("Identified changes in ProductAndItemSolr on PRODUCT_PUBLISH : {}", fieldValueObjectMap);
            SolrInputDocument solrInputDocument = CommonUtil
                .getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductPublish(solrProductAndItem,
                    fieldValueObjectMap);
            solrInputDocumentList.add(solrInputDocument);
          }
        }
        if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
          cloudSolrClient.add(solrInputDocumentList);
        }
      }
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .debug("end indexing master data changes : {}", productDomainEventModel.getProductCode());
      this.reindexL3CollectionProductDomainEventModel(productDomainEventModel, productAndTotalScoreMap);
    } catch (SolrServerException | IOException | SolrException e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .error("{}#applyMasterDataChanges : {}", productDomainEventModel.getProductCode(), e.getMessage(), e);
    }
  }

  @Override
  public void applyMasterDataDetailWithProductAndItems(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProducts,  boolean l3AtomicUpdateEnabled)
      throws IOException, SolrServerException {
    LOGGER.debug("Inside applyMasterDataDetailWithProductAndItems :{}", masterDataDetailWithProducts);
    List<String> productSkus = new ArrayList<String>();
    try {
      List<ProductAndItemSolr> productAndItemsSolr = new ArrayList<ProductAndItemSolr>();
      for (ProductAndItemsVO productAndItems : masterDataDetailWithProducts.getProductAndItems()) {
        Product product = productAndItems.getProduct();
        try {
          String productCode = product.getProductCode();
          if (productCode != null) {
            MasterDataProduct masterDataProduct =
                masterDataDetailWithProducts.getMasterDataProducts().get(productCode);
            if (product.isSynchronized()) {
              product.setMasterDataProduct(masterDataProduct);
            } else {
              if (Objects.isNull(product.getMasterDataProduct())) {
                product.setMasterDataProduct(new MasterDataProduct());
              }
              product.getMasterDataProduct().setMasterCatalog(masterDataProduct.getMasterCatalog());
            }
          }
          productSkus.add(product.getProductSku());
          for (Item item : productAndItems.getItems()) {
            String itemCode = item.getItemCode();
            if (itemCode != null && item.isSynchronized()) {
              MasterDataItem masterDataItem =
                  masterDataDetailWithProducts.getMasterDataItems().get(itemCode);
              item.setMasterDataItem(masterDataItem);
            }
          }
          productAndItemsSolr.addAll(this.constructSolrObject(productAndItems, false));
        } catch (Exception e) {
          ProductAndItemSolrIndexerServiceImpl.LOGGER.error(
              "{}#constructMasterDataDetailWithProductAndItems:{}", product.getProductSku(),
              e.getMessage(), e);
        }
      }
      if (CollectionUtils.isNotEmpty(productAndItemsSolr)) {
        List<SolrInputDocument> solrInputDocuments =
            productAndItemsSolr.stream().map(CommonUtil::toSolrInputDocument).collect(toList());
        cloudSolrClient.add(solrInputDocuments);
      }
      setMasterCatalogForProducts(masterDataDetailWithProducts);
      for(ProductAndItemsVO productAndItemsVO : masterDataDetailWithProducts.getProductAndItems()) {
        try {
          this.reindexL3CollectionProductAndItemsVO(productAndItemsVO, l3AtomicUpdateEnabled);
        } catch (Exception e) {
          ProductAndItemSolrIndexerServiceImpl.LOGGER
              .error("{}#applyMasterDataDetailWithProductAndItems failed to reindex L3 collection:{}", productSkus,
                  e.getMessage(), e);
        }
      }
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error(
          "{}#applyMasterDataDetailWithProductAndItems:{}", productSkus, e.getMessage(), e);
      throw e;
    }
  }

  @Override
  public void applyProduct(Product product, boolean l3AtomicUpdateEnabled) throws Exception {
    try {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("start indexing product :{}",
          product.getProductSku());
      if (eligibleForL4Reindex(product.getMerchantCode(), eligibleSellersForL4Reindex)) {
        try {
          List<ProductAndItemSolr> productAndItemSolrs =
              this.productItemSolrRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(
                  product.getStoreId(), product.getProductSku(), product.getMerchantCode());
          for (ProductAndItemSolr productAndItemSolr : productAndItemSolrs) {
            this.productAndItemConstructorService.constructProduct(productAndItemSolr, product,
                true);
          }
          if (CollectionUtils.isNotEmpty(productAndItemSolrs)) {
            List<SolrInputDocument> solrInputDocuments =
                productAndItemSolrs.stream().map(CommonUtil::toSolrInputDocument).collect(toList());
            cloudSolrClient.add(solrInputDocuments);
          }
          ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("end indexing product :{}",
              product.getProductSku());
        } catch (Exception e) {
          log.error("Error while adding product to L4 solr, product-sku : {} ",
              product.getProductSku(), e);
        }
      }
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .debug("start indexing product for L3 collection :{}", product.getProductSku());
      this.reindexL3CollectionProduct(product, l3AtomicUpdateEnabled);
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error("{}#applyProductSolr:{}",
          product.getProductSku(), e.getMessage(), e);
      throw e;
    }
  }

  @Override
  public void applyProductAndItems(ProductAndItemsVO productAndItems, boolean l3AtomicUpdateEnabled) throws Exception {
    try {
      if (productAndItems != null && productAndItems.getProduct() != null) {
        ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("start indexing product and item :{}",
            productAndItems.getProduct().getProductSku());
        if (eligibleForL4Reindex(productAndItems.getProduct().getMerchantCode(),
            eligibleSellersForL4Reindex)) {
          List<ProductAndItemSolr> productAndItemsSolr =
              this.constructSolrObject(productAndItems, true);
          try {
            if (CollectionUtils.isNotEmpty(productAndItemsSolr)) {
              List<SolrInputDocument> solrInputDocuments =
                  productAndItemsSolr.stream().map(CommonUtil::toSolrInputDocument)
                      .collect(toList());
              cloudSolrClient.add(solrInputDocuments);
            }
            log.info("L4 solr reindex is success for product-sku :{} ",
                productAndItems.getProduct().getProductSku());
          } catch (Exception e) {
            log.error("Error while adding product to L4 solr, product-sku : {} ",
                productAndItems.getProduct().getProductSku(), e);
          }
        }
        try {
          this.reindexL3CollectionProductAndItemsVO(productAndItems, l3AtomicUpdateEnabled);
        } catch (Exception e) {
          ProductAndItemSolrIndexerServiceImpl.LOGGER
              .error("{}#applyProductAndItemsSolr for L3 collection:{}", productAndItems.getProduct().getProductSku(),
                  e.getMessage());
          throw e;
        }
      }
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error("{}#applyProductAndItemsSolr:{}",
          productAndItems.getProduct().getProductSku(), e.getMessage());
      throw e;
    }
  }

  private boolean eligibleForL4Reindex(String merchantCode,
      Set<String> eligibleSellersForL4Reindex) {
    if (CollectionUtils.isNotEmpty(eligibleSellersForL4Reindex)) {
      boolean eligible = eligibleSellersForL4Reindex.contains(merchantCode);
      if (eligible) {
        log.info("Eligible for L4 reindex {} ", merchantCode);
      } else {
        log.info("L4 reindex skipped for merchantCode {} ", merchantCode);
      }
      return eligible;
    } else {
      return true;
    }
  }

  @Override
  public void deleteOfflineItemByMerchantCode(String storeId, String merchantCode) throws Exception {
    try {
      List<ProductAndItemSolr> productAndItemSolrs =
          this.productItemSolrRepository.findAllIdsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(storeId, merchantCode);
      this.productItemSolrRepository.deleteOfflineItemByItemIds(
          productAndItemSolrs.stream().map(ProductAndItemSolr::getId).collect(toList()));
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error("{}#deleteOfflineItem by merchantCode:{}", merchantCode, e);
      throw e;
    }
  }

  @Override
  public void indexMerchantPromoDiscountItem(Item item, boolean skuStateChange) throws Exception {
    SolrInputDocument solrInputDocument =
        CommonUtil.toMerchantPromoDiscountSolrInputDocument(item, skuStateChange, this.solrStringDelimiter);
    this.productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
  }

  @Override
  public void indexMerchantPromoDiscountItemPickupPoint(ItemPickupPoint item, boolean activated) throws Exception {
    SolrInputDocument solrInputDocument;
    if (activated) {
      solrInputDocument =
          CommonUtil.getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(Arrays.asList(item));
    } else {
      List<ItemPickupPoint> itemPickupPoints =
          itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(item.getStoreId(), item.getItemSku());
      solrInputDocument =
          CommonUtil.getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(itemPickupPoints);
    }
    if (Objects.nonNull(solrInputDocument)) {
      productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
    }
  }

  private List<ProductAndItemSolr> constructSolrObject(ProductAndItemsVO productAndItems,
      boolean getMasterData) {
    List<ProductAndItemSolr> productAndItemsSolr = new ArrayList<ProductAndItemSolr>();
    checkArgument(!productAndItems.getItems().isEmpty(),
        ProductAndItemSolrIndexerServiceImpl.ITEM_MUST_NOT_BE_EMPTY);
    List<ProductAndItemSolr> productAndItemFromSolr = new ArrayList<>();
    try {
      productAndItemFromSolr = this.productItemSolrRepository
          .findByStoreIdAndProductSku(productAndItems.getProduct().getStoreId(),
              productAndItems.getProduct().getProductSku(),
            productAndItems.getProduct().getMerchantCode());
    } catch (Exception e) {
      log.error("Exception caught while getting data from L4 Solr productSku : {} ",
          productAndItems.getProduct().getProductSku(), e);
    }
    Map<String, ProductAndItemSolr> mapOfProductAndItemSolr =
        new HashMap<String, ProductAndItemSolr>();
    if (CollectionUtils.isNotEmpty(productAndItemFromSolr)) {
      for (ProductAndItemSolr productAndItemSolr : productAndItemFromSolr) {
        mapOfProductAndItemSolr.put(productAndItemSolr.getId(), productAndItemSolr);
      }
    }
    for (Item item : productAndItems.getItems()) {
      ProductAndItemSolr productItemSolr = mapOfProductAndItemSolr.get(item.getItemSku());
      if (productItemSolr == null) {
        productItemSolr = new ProductAndItemSolr();
      }
      this.productAndItemConstructorService.constructItem(productItemSolr, item, getMasterData);
      this.productAndItemConstructorService.constructProduct(productItemSolr, productAndItems.getProduct(), getMasterData);
      productAndItemsSolr.add(productItemSolr);
    }
    return productAndItemsSolr;
  }

  private ProductAndItemSolr createNewSolrObject(Product product, Item item) {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    if (product != null) {
      this.productAndItemConstructorService.constructProduct(productAndItemSolr, product, true);
    }
    this.productAndItemConstructorService.constructItem(productAndItemSolr, item, true);
    return productAndItemSolr;
  }

  private void saveForRetry(String storeId, String itemSku, String errorMessage) {
    try {
      // this.solrErrorService.save(new SolrErrorHistory(storeId, itemSku, errorMessage));
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER.error("{}#saveForRetry:{}", itemSku,
          e.getMessage(), e);
    }
  }

  @Override
  public void updateItemsInSolr(String storeId, String productSku, boolean isSuspended,
      Map<String, Boolean> archiveItems, boolean productArchivedFlag, List<Item> notPermanentDeletedItems,
    String merchantCode) {
    List<ProductAndItemSolr> productAndItemSolrs =
        productItemSolrRepository.findByStoreIdAndProductSku(storeId, productSku, merchantCode);
    List<String> itemSkusNotDeleted = notPermanentDeletedItems.stream().map(Item::getItemSku).collect(toList());
    List<SolrInputDocument> solrInputDocumentList =
        Optional.ofNullable(productAndItemSolrs).orElse(new ArrayList<>()).stream()
            .filter(
                productAndItemSolr -> !addDeleteVariantSwitch || (!isSuspended && itemSkusNotDeleted.contains(
                    productAndItemSolr.getItemSku())))
            .map(productAndItemSolr -> CommonUtil
            .getSolrInputDocumentForProductAndItemSolrFlagsUpdate(productAndItemSolr, isSuspended, archiveItems))
            .collect(toList());
    for (SolrInputDocument solrInputDocument : solrInputDocumentList) {
      try {
        this.cloudSolrClient.add(solrInputDocument);
      } catch (SolrServerException | IOException | SolrException e) {
        ProductAndItemSolrIndexerServiceImpl.LOGGER
            .error("Exception caught while updating documents to ProductAndItemSolr:{}", solrInputDocument, e);
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForProductSolrFlagsUpdate(isSuspended, productArchivedFlag);
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
        new ProductAndItemEventModel(productSku, fieldsAndValues, merchantCode));
    } else {
      ProductSolr productSolr = this.productSolrRepository.findByProductSku(null, productSku);
      SolrInputDocument solrInputDocumentL3Collection =
          CommonUtil.getSolrInputDocumentForProductSolrFlagsUpdate(productSolr, isSuspended, productArchivedFlag);
      try {
        this.cloudSolrClientL3.add(solrInputDocumentL3Collection);
      } catch (SolrServerException | IOException | SolrException e) {
        ProductAndItemSolrIndexerServiceImpl.LOGGER
            .error("Exception caught while updating document in ProductL3 : {}", solrInputDocumentL3Collection, e);
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
  }

  @Override
  public void updateItemSyncStatusForFulfillmentByBlibli(String storeId, String itemSku, String linkedPartnerCode)
    throws Exception {
    GdnPreconditions.checkArgument(Objects.nonNull(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(linkedPartnerCode), ErrorMessages.BLANK_LINKED_PARTNER_CODE);

    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, itemSku);
    solrInputDocument.setField(SolrFieldNames.STORE_ID, storeId);
    solrInputDocument
      .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument
      .setField(SolrFieldNames.LINKED_PARTNERS, Collections.singletonMap(SolrConstants.ADD_CLAUSE, linkedPartnerCode));

    this.productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
  }

  @Override
  @Async
  public void updateSolrOnToggleArchiveItemAction(Item item) {
    checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      SolrInputDocument solrInputDocument =
          CommonUtil.getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction(item, solrStringDelimiter);
      this.productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on archival action , itemSku :{}", item.getItemSku(), e);
    }
  }

  @Override
  @Async
  public void deleteItemsFromSolrAfterPostLiveRejection(List<String> itemSkuList) {
    try {
      checkArgument(CollectionUtils.isNotEmpty(itemSkuList), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
      cloudSolrClient.deleteById(itemSkuList);
    } catch (Exception e) {
      LOGGER.error("Exception caught while deleting records for items : {} ", itemSkuList, e);
    }
  }

  @Override
  @Async
  public void deleteSolrDocumentByProductSkuInL4Solr(String productSku) {
    try {
      if (StringUtils.isNotEmpty(productSku)) {
        String query = SolrFieldNames.PRODUCT_SKU + SolrConstants.COLON + productSku;
        cloudSolrClient.deleteByQuery(query);
      }
    } catch (Exception e) {
      log.error("Exception caught while deleting Solr documents by productSku: {} ", productSku, e);
      throw new SolrCustomException(e.getMessage(), e);
    }
  }

  @Override
  @Async
  public void deleteProductsFromSolrAfterPostLiveRejection(Set<String> productSkuList) {
    try {
      checkArgument(CollectionUtils.isNotEmpty(productSkuList), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
      productSolrRepository.deleteSolrDocumentsByListOfProductSku(productSkuList);
    } catch (Exception e) {
      LOGGER.error("Exception caught while deleting records for products : {} ", productSkuList, e);
    }
  }

  @Override
  @Async
  public void updateSolrOnSyncUnsyncAction(ProductAndItemsVO productAndItemsVO) {
    checkArgument(CollectionUtils.isNotEmpty(productAndItemsVO.getItems()), ErrorMessages.ITEMS_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(productAndItemsVO);
      if (CollectionUtils.isNotEmpty(solrInputDocuments)) {
        this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
      }
      if (eventBasedSolrUpdateEnable) {
        Map<String, Object> fieldsAndValues = CommonUtil
            .getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
            ProductDomainEventName.UPDATE_TO_SOLR, productAndItemsVO.getProduct().getProductSku(), fieldsAndValues);
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
          productAndItemsVO.getProduct().getProductSku(),
          new ProductAndItemEventModel(productAndItemsVO.getProduct().getProductSku(),
            fieldsAndValues, productAndItemsVO.getProduct().getMerchantCode()));
      } else {
        SolrInputDocument solrInputDocumentForL3Collection =
            CommonUtil.getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncAction(productAndItemsVO.getProduct());
        this.cloudSolrClientL3.add(solrInputDocumentForL3Collection);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on sync/unsync changes , productSku :{}",
          productAndItemsVO.getProduct().getProductSku(), e);
    }
  }

  @Override
  @Async
  public void updateSolrOnPriceChange(List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrInputDocumentForAtomicUpdateOnPriceChanges(items, solrStringDelimiter);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on price changes , items :{}", items, e);
    }
  }

  @Override
  @Async
  public void updateSolrOnPromoBundlingFlagChange(List<Item> itemList, boolean promoBundling) {
    checkArgument(CollectionUtils.isNotEmpty(itemList), ErrorMessages.ITEMS_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrDocumentListForAtomicUpdateOnPromoBundlingFlagChange(itemList, promoBundling);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER
          .error("Exception caught while doing atomic update on master catalog changes , itemList :{}", itemList, e);
    }
  }

  @Override
  @Async
  public void updateSolrOnPromoFlagChangeByItemSkus(Map<String, List<String>> productSkuMap, boolean activated,
      String fieldName, Map<String, String> productSkuAndMerchantCodeMap) {
    checkArgument(MapUtils.isNotEmpty(productSkuMap), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    List<SolrInputDocument> solrInputDocumentsL3 = new ArrayList<>();
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    Map<String, Map<String, Object>> productSkuAndSolrUpdateFeildsMap = new HashMap<>();
    try {
      if (activated) {
        getAndSetSolrInputDocumentsWhileActivatingPromo(productSkuMap, solrInputDocumentsL3, solrInputDocuments,
            fieldName, productSkuAndSolrUpdateFeildsMap, productSkuAndMerchantCodeMap);
      } else {
        getAndSetSolrInputDocumentsWhileDeactivatingPromo(productSkuMap, solrInputDocumentsL3, solrInputDocuments,
            fieldName, productSkuAndSolrUpdateFeildsMap, productSkuAndMerchantCodeMap);
      }
      if (CollectionUtils.isNotEmpty(solrInputDocuments)) {
        this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
      }
      if (eventBasedSolrUpdateEnable) {
        for (String productSku : productSkuAndSolrUpdateFeildsMap.keySet()) {
          log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
              ProductDomainEventName.UPDATE_TO_SOLR, productSku, productSkuAndSolrUpdateFeildsMap.get(productSku));
          kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
            new ProductAndItemEventModel(productSku,
              productSkuAndSolrUpdateFeildsMap.get(productSku), productSkuAndMerchantCodeMap.get(productSku)));
        }
      }
      else if (CollectionUtils.isNotEmpty(solrInputDocumentsL3)) {
        this.productSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocumentsL3);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on promo bundling changes , productSkuMap :{}",
          productSkuMap, e);
    }
  }

  private void getAndSetSolrInputDocumentsWhileDeactivatingPromo(Map<String, List<String>> productSkuMap,
      List<SolrInputDocument> solrInputDocumentsL3, List<SolrInputDocument> solrInputDocuments, String fieldName,
      Map<String, Map<String, Object>> productSkuAndSolrUpdateFeildsMap, Map<String, String> productSkuAndMerchantCodeMap) {
    for (String productSku : productSkuMap.keySet()) {
      List<ItemPickupPoint> itemPickupPoints =
          itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              productSkuMap.get(productSku));
      if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
        Map<String, List<ItemPickupPoint>> itemSkuMap =
            itemPickupPoints.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
        if (StringUtils.isNotBlank(fieldName)) {
          getAndSetL4SolrInputDocuments(solrInputDocuments, itemSkuMap, fieldName,
              productSkuAndMerchantCodeMap.get(productSku));
        }
        if (eventBasedSolrUpdateEnable) {
          productSkuAndSolrUpdateFeildsMap.put(productSku, CommonUtil
              .getFieldsAndValuesL3ForPromoUpdate(itemSkuMap, new ArrayList<>(), populateLabelForUpcomingPromo, populateLabelForPwpPromo));
        } else {
          solrInputDocumentsL3.add(
              CommonUtil.getSolrInputDocumentL3ForPromoUpdate(itemSkuMap, new ArrayList<>(), productSku,
                  populateLabelForUpcomingPromo, populateLabelForPwpPromo));
        }
      }
    }
  }

  private void getAndSetL4SolrInputDocuments(List<SolrInputDocument> solrInputDocuments,
      Map<String, List<ItemPickupPoint>> itemSkuMap, String fieldName, String merchantCode) {
    for (String itemSku : itemSkuMap.keySet()) {
      boolean activated = false;
      if (SolrFieldNames.PROMO_BUNDLING.equalsIgnoreCase(fieldName)) {
        activated = itemSkuMap.get(itemSku).stream().filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete())
            .anyMatch(itemPickupPoint -> itemPickupPoint.isPromoBundling());
      } else if (SolrFieldNames.MERCHANT_PROMO_DISCOUNT.equalsIgnoreCase(fieldName)) {
        activated = itemSkuMap.get(itemSku).stream().filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete())
            .anyMatch(itemPickupPoint -> itemPickupPoint.isMerchantPromoDiscount());
      }
      solrInputDocuments.add(
          CommonUtil.getSolrInputDocumentForAtomicUpdateOnPromoFlagChangeByItemSku(itemSku, activated, fieldName,
              merchantCode));
    }
  }

  private void getAndSetSolrInputDocumentsWhileActivatingPromo(Map<String, List<String>> productSkuMap,
      List<SolrInputDocument> solrInputDocumentsL3, List<SolrInputDocument> solrInputDocuments, String fieldName,
      Map<String, Map<String, Object>> productSkuAndSolrUpdateFeildsMap,
      Map<String, String> productSkuAndMerchantCodeMap) {
    for (String productSku : productSkuMap.keySet()) {
      if (CollectionUtils.isNotEmpty(productSkuMap.get(productSku))) {
        if (eventBasedSolrUpdateEnable) {
          productSkuAndSolrUpdateFeildsMap.put(productSku, CommonUtil
              .getFieldsAndValuesL3ForPromoUpdate(new HashMap<>(), productSkuMap.get(productSku),
                  populateLabelForUpcomingPromo, populateLabelForPwpPromo));
        } else {
          solrInputDocumentsL3.add(
              CommonUtil.getSolrInputDocumentL3ForPromoUpdate(new HashMap<>(), productSkuMap.get(productSku),
                  productSku, populateLabelForUpcomingPromo, populateLabelForPwpPromo));
        }
        if (StringUtils.isNotBlank(fieldName)) {
          List<String> itemSkus =
              productSkuMap.values().stream().flatMap(itemSku -> itemSku.stream()).collect(toList());
          solrInputDocuments.addAll(
              CommonUtil.getSolrDocumentListForAtomicUpdateOnPromoFlagChangeByItemSkus(itemSkus, true, fieldName,
                  productSkuAndMerchantCodeMap.get(productSku)));
        }
      }
    }
  }

  @Override
  @Async
  public void updateSolrOnMasterCatalogChanges(Product product, List<Item> items) {
    checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments = CommonUtil
          .getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChanges(product, items, solrStringDelimiter);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
      if (eventBasedSolrUpdateEnable) {
        Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForMasterCatalogAtomicUpdateL3Collection(product,
            solrStringDelimiter);
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
            ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), fieldsAndValues);
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(),
          new ProductAndItemEventModel(product.getProductSku(), fieldsAndValues, product.getMerchantCode()));
      } else {
        SolrInputDocument solrInputDocumentL3Collection =
            CommonUtil.getSolrInputDocumentForMasterCatalogAtomicUpdateL3Collection(product, solrStringDelimiter);
        this.cloudSolrClientL3.add(solrInputDocumentL3Collection);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on master catalog changes , productSku :{}",
          product.getProductSku(), e);
    }
  }

  @Override
  @Async
  public void updateSolrOnSalesCatalogChanges(Product product, List<Item> items) {
    checkArgument(product != null, ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments = CommonUtil
          .getSolrInputDocumentsForAtomicUpdateOnSalesCatalogChanges(product, items, solrStringDelimiter);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
      if (eventBasedSolrUpdateEnable) {
        Map<String, Object> fieldsAndValues = CommonUtil
            .getFieldsAndValuesForAtomicUpdateOnProductSalesCatalogChanges(product, solrStringDelimiter);
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
            ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), fieldsAndValues);
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,product.getProductSku(),
          new ProductAndItemEventModel(product.getProductSku(), fieldsAndValues, product.getMerchantCode()));
      } else {
        SolrInputDocument solrInputDocumentForL3Collection =
            CommonUtil.getSolrInputDocumentsForAtomicUpdateOnProductSalesCatalogChanges(product, solrStringDelimiter);
        this.cloudSolrClientL3.add(solrInputDocumentForL3Collection);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on Sales catalog changes , productSku :{}",
          product.getProductSku(), e);
    }
  }


  @Override
  @Async
  public void updateSolrOnSalesCatalogChangesForProductList(List<Product> products, List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(products), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(CollectionUtils.isNotEmpty(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    Map<String, Product> productMap =
        products.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
    try {
      List<SolrInputDocument> solrInputDocuments = CommonUtil
          .getSolrInputDocumentsForL3ListAtomicUpdateOnSalesCatalogChanges(productMap, items, solrStringDelimiter);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
      if (eventBasedSolrUpdateEnable) {
        for (String productSku : productMap.keySet()) {
          Map<String, Object> fieldsAndValues = CommonUtil
              .getFieldsAndValuesForAtomicUpdateOnProductSalesCatalogChanges(productMap.get(productSku), solrStringDelimiter);
          log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
              ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
          kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
            new ProductAndItemEventModel(productSku, fieldsAndValues,
              products.stream().findFirst().map(Product::getMerchantCode)
                .orElse(items.stream().findFirst().map(Item::getMerchantCode).orElse(null))));
        }
      } else {
        List<SolrInputDocument> solrInputDocumentForL3Collection = CommonUtil
            .getSolrInputDocumentsForAtomicUpdateOnProductsSalesCatalogChanges(new ArrayList<>(productMap.values()),
                solrStringDelimiter);
        this.cloudSolrClientL3.add(solrInputDocumentForL3Collection);
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on Sales catalog changes , productSkus :{}",
          productMap.keySet(), e);
    }
  }

  @Override
  @Async
  public void updateSolrOnItemViewConfigChanges(List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments = items.stream()
          .map(item -> CommonUtil.getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges(item, solrStringDelimiter))
          .collect(toList());
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on item view config changes , itemList :{}",
          items, e);
    }
  }

  @Override
  @Async
  public void updateSolrOnItemViewConfigChangesByItemPickupPoint(List<ItemPickupPoint> itemPickupPoints) {
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments = itemPickupPoints.stream().map(
          itemPickupPoint -> CommonUtil.getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges(itemPickupPoint,
              solrStringDelimiter)).collect(toList());
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on item view config changes , itemPickupPointList :{}",
          itemPickupPoints, e);
    }
  }

  @Override
  public void updateSolrOnContentChange(List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments =
          items.stream().map(CommonUtil::getSolrInputDocumentForAtomicUpdateOnContentChange)
              .collect(toList());
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on content change , itemList :{} ", items, e);
    }
  }

  @Override
  public void takeDownL3(Product product) {
    ProductSolr productSolr =
        this.productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku());
    try {
      if (Objects.nonNull(productSolr) && StringUtils.isNotBlank(productSolr.getProductSku())) {
        if (eventBasedSolrUpdateEnable) {
          Map<String, Object> fieldsAndValues = new HashMap<>();
          fieldsAndValues.put(SolrFieldNames.MARK_FOR_DELETE,
              Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isMarkForDelete()));
          log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
              ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), fieldsAndValues);
          kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(),
            new ProductAndItemEventModel(product.getProductSku(), fieldsAndValues, product.getMerchantCode()));
        } else {
          SolrInputDocument solrInputDocumentL3Collection =
              CommonUtil.getUpdateMarkForDelete(productSolr, product.isMarkForDelete());
          this.cloudSolrClientL3.add(solrInputDocumentL3Collection);
        }
      } else {
        this.productL3SolrReindexStatusService.insertProductSkuToReindexStatusCollection(
            toProductL3SolrReindexStatus(product, ProductReindexStatus.FULL_REINDEX_PENDING_PRODUCTS));
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on product, product : {} ", product, e);
    }
  }

  @Override
  public void reindexProductToL3Collection(Product product, List<Item> itemList, boolean productInStock) {
    try {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .debug("start indexing product for new L3 collection:{}", product.getProductSku());
      ProductSolr productSolr = this.productSolrRepository.findByProductSku(product.getMerchantCode(), product.getProductSku());
      this.productSolrConstructorService.constructProduct(productSolr, product, true);
      if (Objects.nonNull(productSolr)) {
        List<ItemPickupPoint> itemPickupPoints =
            itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
                product.getProductSku());
        SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
        CommonUtil.generateL3AggregateDataFromItems(product, itemList, solrInputDocument, itemPickupPoints,
            productInStock, populateLabelForUpcomingPromo, populateLabelForPwpPromo);
        cloudSolrClientL3.add(solrInputDocument);
      }
      ProductAndItemSolrIndexerServiceImpl.LOGGER.debug("end indexing product :{}", product.getProductSku());
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
          .error("{}#applyProductSolr:{}", product.getProductSku(), e.getMessage(), e);
    }
  }

  @Override
  public void updateWholesalePriceActivatedFlag(String itemSku, Boolean wholesalePriceActivated) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, itemSku);
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, wholesalePriceActivated));
    try {
      cloudSolrClient.add(solrInputDocument);
    } catch (SolrServerException | IOException | SolrException e) {
      LOGGER.error("Exception caught while doing atomic update on wholesalePriceActivated changes , itemSku :{}",
          itemSku, e);
    }
  }

  @Override
  @Async
  public void updateSolrOnPriceAndWholesalePriceFlagChange(Item item, Boolean wholesalePriceActivated) {
    try {
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrInputDocumentForAtomicUpdateOnPriceChanges(Arrays.asList(item), solrStringDelimiter);
      solrInputDocuments.get(0).setField(SolrFieldNames.ID, item.getItemSku());
      solrInputDocuments.get(0)
          .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      if (Objects.nonNull(wholesalePriceActivated)) {
        solrInputDocuments.get(0).setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, wholesalePriceActivated));
      }
      this.productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocuments.get(0));
      updatePriceInL3Collection(item.getStoreId(), item.getProductSku(), item);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on price amd wholesale flag changes , item :{}",
          item.getItemSku(), e);
    }
  }

  private void updatePriceInL3Collection(String storeId, String productSku, Item item) {
    List<Item> items =
        productHelperService.getCachedItemsByProductSkuWithoutOverridingL5Data(storeId, productSku, Arrays.asList(item.getItemSku()));
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByItemSkusAndDelivery(storeId,
        items.stream().map(Item::getItemSku).collect(toList()), true);
    objectConverter.overrideL4DetailsFromL5(items, itemPickupPoints);
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForPriceAtomicUpdateL3(item, items);
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
        new ProductAndItemEventModel(productSku, fieldsAndValues, item.getMerchantCode()));
    } else {
      SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentForPriceAtomicUpdateL3(item, items);
      try {
        this.cloudSolrClientL3.add(solrInputDocument);
      } catch (SolrServerException | IOException | SolrException e) {
        ProductAndItemSolrIndexerServiceImpl.LOGGER.error("Exception caught while updating document in ProductL3 : {} ",
            e);
        throw new SolrCustomException(e.getMessage(), e);
      }
    }
  }

  @Override
  public ProductSolr getProductSolrByProductSku(String productSku) throws Exception {
    return productSolrRepository.findByProductSku(null, productSku);
  }

  @Override
  public void updateSolrOnPristineChanges(List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    List<String> itemSkus = new ArrayList<>();
    try {
      itemSkus = items.stream().map(Item::getItemSku).collect(toList());
      LOGGER.info("Start atomic update of items : {}, {}", items.size(), itemSkus);
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrInputDocumentForAtomicUpdateOnPristineChange(items);
      this.productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on pristine changes , itemList :{}", itemSkus, e);
    }
  }

  @Override
  public ProductPickupPointListResponse getPickupPointCodesByProductSku(String storeId, String productSku)
      throws Exception {
    ProductSolr productSolr = productSolrRepository.findOneByProductSkuAndMarkForDeleteFalse(productSku);
    checkArgument(CollectionUtils.isNotEmpty(productSolr.getPickupPointCodes()),
        ErrorMessages.EMPTY_ITEM_LIST_FOR_PRODUCT_SKU);
    return ProductPickupPointListResponse.builder().pickupPointCodes(new HashSet<>(productSolr.getPickupPointCodes()))
        .build();
  }

  @Override
  public void productListingAtomicUpdate(Product product, List<Item> items) throws Exception {
    List<String> updateItemSkus = items.stream().map(Item::getItemSku).collect(toList());
    List<SolrInputDocument> solrInputDocuments =
        CommonUtil.getSolrInputDocumentsForListingUpdate(items, product.getProductType(), solrStringDelimiter);
    productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    List<Item> cachedItems = productHelperService
        .getCachedItemsByProductSku(product.getStoreId(), product.getProductSku(),
            items.stream().map(Item::getItemSku).collect(toList()));
    cachedItems.addAll(items);
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = CommonUtil.getFieldsAndValuesForListingUpdate(cachedItems, product.isArchived());
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(),
        new ProductAndItemEventModel(product.getProductSku(), fieldsAndValues, product.getMerchantCode()));
    } else {
      SolrInputDocument solrInputDocumentL3 = CommonUtil
          .getL3SolrInputDocumentsForListingUpdate(product.getProductSku(), cachedItems, product.isArchived());
      cloudSolrClientL3.add(solrInputDocumentL3);
    }
  }

  @Override
  public void pickupPointCodesAtomicUpdate(Item item) {
    SolrInputDocument solrInputDocument = CommonUtil.getpickupPointCodeAtomicUpdateInputDocument(item);
    try {
      productItemSolrRepository.executeSolrDocumentAtomicUpdate(solrInputDocument);
    } catch (Exception e) {
      LOGGER.error("Exception whil updating pickupPointCode, itemSku: {}, {} ", item.getItemSku(), e);
    }
  }

  @Override
  public Product pickupPointCodesUpdateAndSolrPublish(List<Item> currentItems, boolean isDifferentLocation) {
    List<Item> items = new ArrayList<>();
    if (isDifferentLocation) {
      items = productHelperService
          .getCachedItemsByProductSku(currentItems.get(0).getStoreId(), currentItems.get(0).getProductSku(),
              currentItems.stream().map(Item::getItemSku).collect(toList()));
      items.addAll(currentItems);
    } else {
      items.addAll(currentItems);
    }
    Product product = productService.findByStoreIdAndProductSku(items.get(0).getStoreId(), items.get(0).getProductSku());
    product.setPickupPointCodes(items.stream().filter(item -> !item.isPermanentDelete()).map(Item::getPickupPointCode)
        .collect(Collectors.toSet()));
    Product savedProduct = productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY);
    updateProductAndItemDetailsInSolr(product, items, true);
    return savedProduct;
  }

  @Override
  public void offlineItemPriceAtomicUpdate(String itemSku, List<OfflineItem> offlineItems) {
    List<String> offlinePrices = new ArrayList<>();
    for (OfflineItem offlineItem : offlineItems) {
      String offlinePrice = offlineItem.getPickupPointCode() + this.solrStringDelimiter + offlineItem.getListPrice()
          + this.solrStringDelimiter + offlineItem.getOfferPrice();
      offlinePrices.add(offlinePrice);
    }
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, itemSku);
    solrInputDocument
        .setField(SolrFieldNames.OFFLINE_PRICES, Collections.singletonMap(SolrConstants.SET_CLAUSE, offlinePrices));
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    try {
      cloudSolrClient.add(solrInputDocument);
    } catch (IOException | SolrServerException | SolrException e) {
      LOGGER
          .error("Error while updating offile prices for itemSku : {}, offlinePrices : {} ", itemSku, offlinePrices, e);
    }
  }

  @Override
  public void applyPendingProductReindex(List<Product> productList,List<String> productSkuList){
    LOGGER.debug("Inside applyPendingProductReindex product list size :{}",
      productList.size());
    try {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("starting re-indexing product for L3 collection");
        reindexPendingL3CollectionProduct(productList);
    } catch (Exception e) {
      ProductAndItemSolrIndexerServiceImpl.LOGGER
        .error("{}#applyPendingProductReindex Failed to reindex pending L3 collection:{}",
          e.getMessage(), e);
    }
  }

  @Override
  public void reindexOfflineItemAndCncActivatedFlag(String storeId, List<Item> items) throws Exception {
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    for (Item item : items) {
      List<String> offlinePrices =
          productAndItemConstructorService.getOfflinePricesByStoreIdAndItemSku(storeId, item.getItemSku());
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
      solrInputDocument.setField(SolrFieldNames.OFFLINE_PRICES,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, offlinePrices));
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isCncActivated()));
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
      solrInputDocuments.add(solrInputDocument);
    }
    if (CollectionUtils.isNotEmpty(solrInputDocuments)) {
      productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    }
  }

  private void reindexL3CollectionProduct(Product product, boolean l3AtomicUpdateEnabled) throws Exception {
    //get product solr document using product skus
    ProductSolr productSolr = this.productSolrRepository
        .findByProductSku(product.getMerchantCode(), product.getProductSku());
    boolean productSolrFound = StringUtils.isNotEmpty(productSolr.getProductSku());

    //construct product solr with the latest info
    this.productSolrConstructorService.constructProduct(productSolr, product, true);

    //get items by product sku
    List<Item> items = productHelperService
        .getCachedItemsByProductSku(product.getStoreId(), product.getProductSku(), new ArrayList<>());

    //get item pickup points by productSku
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            product.getProductSku());

    //get stock from inventory when l3AtomicUpdateEnabled = false or productSku in product Solr is empty
    Boolean productHasStock = null;
    if (!l3AtomicUpdateEnabled || !productSolrFound) {
      boolean isPreOrder = preOrderQuotaSwitch && Objects.nonNull(product.getPreOrder()) &&
          ProductAndItemsUtil.isPreOrderActive(product.getPreOrder().getPreOrderDate());
      productHasStock = inventoryOutbound.isProductInStock(product.getProductSku(), isPreOrder);
    }

    //generate solr input document
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
    CommonUtil.generateL3AggregateDataFromItems(product, items, solrInputDocument, itemPickupPoints,
        BooleanUtils.toBooleanDefaultIfNull(productHasStock, false), populateLabelForUpcomingPromo,
        populateLabelForPwpPromo);

    //do atomic update if l3AtomicUpdateEnabled = true else do full reindex
    if (l3AtomicUpdateEnabled && productSolrFound) {
      solrInputDocument =
          CommonUtil.transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocument(solrInputDocument,
              productHasStock);
    }

    cloudSolrClientL3.add(solrInputDocument);

    ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("End indexing product for L3 collection : {}", product.getProductSku());
  }

  private void reindexL3CollectionProductAndItemsVO(ProductAndItemsVO productAndItems, boolean l3AtomicUpdateEnabled) throws Exception {
    ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("start indexing product for L3 collection :{}", productAndItems.getProduct().getProductSku());

    //get product solr document using product skus
    ProductSolr productSolr = this.productSolrRepository
        .findByProductSku(productAndItems.getProduct().getMerchantCode(), productAndItems.getProduct().getProductSku());
    boolean productSolrFound = StringUtils.isNotEmpty(productSolr.getProductSku());

    //get product by product sku
    Product product = this.productService
        .findByStoreIdAndProductSku(productAndItems.getProduct().getStoreId(),
            productAndItems.getProduct().getProductSku());

    //get items by item skus or product skus
    List<Item> items;
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      items = productHelperService.getCachedItemsByProductSku(productAndItems.getProduct().getStoreId(),
          productAndItems.getProduct().getProductSku(),
          productAndItems.getItems().stream().map(Item::getItemSku).collect(toList()));
      items.addAll(productAndItems.getItems());
    } else {
      items = productHelperService.getCachedItemsByProductSku(productAndItems.getProduct().getStoreId(),
          productAndItems.getProduct().getProductSku(), new ArrayList<>());
    }

    //get item pickup points by product sku
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, productAndItems.getProduct().getProductSku());

    //get stock from inventory when l3AtomicUpdateEnabled = false or productSku in product Solr is empty
    Boolean productHasStock = null;
    if (!l3AtomicUpdateEnabled || !productSolrFound) {
      boolean isPreOrder =
        preOrderQuotaSwitch && Objects.nonNull(product) && Objects.nonNull(product.getPreOrder()) &&
          ProductAndItemsUtil.isPreOrderActive(product.getPreOrder().getPreOrderDate());
       productHasStock = inventoryOutbound.isProductInStock(productAndItems.getProduct().getProductSku(), isPreOrder);
    }

    //construct product solr with latest info
    this.productSolrConstructorService.constructProduct(productSolr, productAndItems.getProduct(), true);

    //generate solr input document
    SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
    CommonUtil.generateL3AggregateDataFromItems(productAndItems.getProduct(), null, solrInputDocument, itemPickupPoints,
        BooleanUtils.toBooleanDefaultIfNull(productHasStock, false), populateLabelForUpcomingPromo,
        populateLabelForPwpPromo);

    //compare and check if product is already deleted
    if (Objects.nonNull(product)
      && Boolean.compare(productAndItems.getProduct().isMarkForDelete(), product.isMarkForDelete())
      != 0) {
      solrInputDocument.setField(SolrFieldNames.MARK_FOR_DELETE, product.isMarkForDelete());
    }

    //do atomic update if l3AtomicUpdateEnabled = true else do full reindex
    if (l3AtomicUpdateEnabled && productSolrFound) {
      solrInputDocument =
          CommonUtil.transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocument(solrInputDocument,
              productHasStock);
    }

    cloudSolrClientL3.add(solrInputDocument);


    ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("End indexing product for L3 collection : {}", productAndItems.getProduct().getProductSku());
  }

  private void reindexL3CollectionProductDomainEventModel(ProductDomainEventModel productDomainEventModel,
      Map<String, Double> productAndTotalScoreMap) throws Exception {
    ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("start indexing product for L3 collection :{}", productDomainEventModel.getProductCode());
    List<ProductSolr> productSolrList =
        this.productSolrRepository.findByProductCode(productDomainEventModel.getProductCode(), null);
    List<SolrInputDocument> solrInputDocumentList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productSolrList)) {
      for (ProductSolr productSolr : productSolrList) {
        Map<String, FieldValueObject> fieldValueObjectMap = this.productSolrConstructorService
            .constructProductFromMasterDataChanges(productSolr, productDomainEventModel, productAndTotalScoreMap);
        if (Objects.nonNull(fieldValueObjectMap)) {
          LOGGER.info("Identified changes in ProductL3 on PRODUCT_PUBLISH : {}", fieldValueObjectMap);
          SolrInputDocument solrInputDocument = CommonUtil
              .getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductPublish(productSolr, fieldValueObjectMap);
          solrInputDocumentList.add(solrInputDocument);
        }
      }
      if (CollectionUtils.isNotEmpty(solrInputDocumentList)) {
        cloudSolrClientL3.add(solrInputDocumentList);
      }
    }
    ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("End indexing product for L3 collection : {}", productDomainEventModel.getProductCode());
  }

  private void setMasterCatalogForProducts(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo) {
    for (ProductAndItemsVO productAndItemsVO : masterDataDetailWithProductAndItemsResponseVo.getProductAndItems()) {
      if (Objects.isNull(productAndItemsVO.getProduct().getMasterCatalog())) {
        MasterCatalog masterCatalog = masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts()
            .get(productAndItemsVO.getProduct().getProductCode()).getMasterCatalog();
        productAndItemsVO.getProduct().setMasterCatalog(masterCatalog);
      }
    }
  }

  private ProductL3SolrReindexStatus toProductL3SolrReindexStatus(Product product,
      ProductReindexStatus productReindexStatus) {
    ProductL3SolrReindexStatus productL3SolrReindexStatus =
        ProductL3SolrReindexStatus.builder().productSku(product.getProductSku())
            .productReindexStatus(productReindexStatus).build();
    productL3SolrReindexStatus.setMarkForDelete(false);
    productL3SolrReindexStatus.setStoreId(product.getStoreId());
    productL3SolrReindexStatus.setCreatedBy(product.getCreatedBy());
    productL3SolrReindexStatus.setUpdatedBy(product.getUpdatedBy());
    productL3SolrReindexStatus.setCreatedDate(product.getCreatedDate());
    productL3SolrReindexStatus.setUpdatedDate(new Date());
    return productL3SolrReindexStatus;
  }

  private void reindexPendingL3CollectionProduct(List<Product> productList) throws Exception {
    ProductSolr productSolr = new ProductSolr();
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    for (Product product : productList) {
      this.productSolrConstructorService.constructProduct(productSolr, product, true);
      boolean isPreOrder = preOrderQuotaSwitch && Objects.nonNull(product.getPreOrder()) &&
          ProductAndItemsUtil.isPreOrderActive(product.getPreOrder().getPreOrderDate());
      boolean productHasStock = inventoryOutbound.isProductInStock(product.getProductSku(), isPreOrder);
      List<Item> items = productHelperService
        .getCachedItemsByProductSku(product.getStoreId(), product.getProductSku(),
          new ArrayList<>());
      List<ItemPickupPoint> itemPickupPoints =
          itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              product.getProductSku());
      SolrInputDocument solrInputDocument = CommonUtil.toSolrInputDocument(productSolr);
      CommonUtil.generateL3AggregateDataFromItems(product, items, solrInputDocument, itemPickupPoints, productHasStock,
          populateLabelForUpcomingPromo, populateLabelForPwpPromo);
      solrInputDocuments.add(solrInputDocument);
      ProductAndItemSolrIndexerServiceImpl.LOGGER
        .debug("Added product to solr client for reindex : {}", product.getProductSku());
    }
    cloudSolrClientL3.add(solrInputDocuments);
  }

  @Override
  @Async
  public void updateSolrOnProductTypeChange(List<Item> items, ProductType productType) {
    checkArgument(Objects.nonNull(items), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    try {
      List<SolrInputDocument> solrInputDocuments =
          CommonUtil.getSolrInputDocumentsForProductTypeUpdate(items, productType);
      productItemSolrRepository.executeSolrDocumentsAtomicUpdate(solrInputDocuments);
    } catch (Exception e) {
      LOGGER.error("Exception caught while doing atomic update on product type update", e);
    }
  }

  @Override
  public void updatePickUpPointAndVariantCountAndCncActivation(ProductAndItemsVO productAndItemsVO,
      List<ItemPickupPoint> itemPickupPoints) {
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = CommonUtil
          .getFieldsAndValuesForUpdatedCNCActivaAndPickupPointAndVariant(itemPickupPoints);
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productAndItemsVO.getProduct().getProductSku(), fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemsVO.getProduct().getProductSku(),
        new ProductAndItemEventModel(productAndItemsVO.getProduct().getProductSku(),
          fieldsAndValues, productAndItemsVO.getProduct().getMerchantCode()));
    } else {
      SolrInputDocument solrInputDocumentL3 =
          CommonUtil.getL3SolrInputDocumentsForUpdatedCNCActivaAndPickupPointAndVariant(productAndItemsVO.getProduct().getProductSku(),
              productAndItemsVO.getItems(), itemPickupPoints);
      try {
        cloudSolrClientL3.add(solrInputDocumentL3);
      } catch (SolrException | SolrServerException | IOException e) {
        LOGGER.error("Exception caught while doing atomic update for L3 Solr Collection : {} productSku : {}",
            productAndItemsVO.getProduct().getProductCode(), productAndItemsVO.getProduct().getProductSku(), e);
      }
    }
  }

  @Override
  public void updateProductDetailsInSolr(List<Product> products) {
    for (Product product : products){
      ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
      productAndItemEventModel.setProductSku(product.getProductSku());
      productAndItemEventModel.setProduct(objectConverterService.convertToProductEventModel(product));
      productAndItemEventModel.setMerchantCode(product.getMerchantCode());
      log.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
          productAndItemEventModel);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel.getProductSku(), productAndItemEventModel);
    }
  }

  @Override
  public void updateProductAndItemDetailsInSolr(Product product, List<Item> items,
      boolean needToOverrideL4DetailsFromL5) {
    ProductAndItemsVO result = new ProductAndItemsVO(product, items);
    ProductAndItemEventModel productAndItemEventModel =
      objectConverterService.convertToProductAndItemEventModel(result);
    productAndItemEventModel.setNeedToOverrideL4DetailsFromL5(needToOverrideL4DetailsFromL5);
    productAndItemEventModel.setMerchantCode(product.getMerchantCode());
    log.info("Publishing event : {}, productAndItemEventModel : {}",
      ProductDomainEventName.UPDATE_TO_SOLR, productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
  }

  @Override
  public void updateDistinctPickupPointCodesAndL5Count(String productSku, Set<String> pickupPointCodes, int l5Count,
    boolean isFbbActivated, String merchantCode) {
    if (eventBasedSolrUpdateEnable) {
      Map<String, Object> fieldsAndValues = new HashMap<>();
      fieldsAndValues.put(SolrFieldNames.PICKUP_POINT_CODES,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
      fieldsAndValues.put(SolrFieldNames.L5_COUNT, l5Count);
      fieldsAndValues.put(SolrFieldNames.FBB_ACTIVATED, isFbbActivated);
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productSku, fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, productSku,
        new ProductAndItemEventModel(productSku, fieldsAndValues, merchantCode));
    } else {
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
      solrInputDocument.setField(SolrFieldNames.L5_COUNT, l5Count);
      try {
        cloudSolrClientL3.add(solrInputDocument);
      } catch (SolrException | SolrServerException | IOException e) {
        LOGGER.error("Wxception while update pickupPointCodes in L3 solr.  productSku : {}, pickupPointCodes : {} ",
            productSku, pickupPointCodes, e);
      }
    }
  }

  @Override
  public void eventBasedAtomicUpdateToSolr(String productSku, String merchantCode,
      Map<String, Object> fieldsAndValues) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    for (String field : fieldsAndValues.keySet()) {
      if (CommonUtil.isDateField(field)) {
        solrInputDocument.setField(field, Collections.singletonMap(SolrConstants.SET_CLAUSE,
            CommonUtil.toDateFromTimestampObject(fieldsAndValues.get(field))));
      } else {
        solrInputDocument.setField(field, fieldsAndValues.get(field));
      }
    }
    try {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
      if (StringUtils.isBlank(merchantCode)) {
        Product product = productService.getProduct(Constants.DEFAULT_STORE_ID, productSku);
        merchantCode = product.getMerchantCode();
      }
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, merchantCode);
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      cloudSolrClientL3.add(solrInputDocument);
    } catch (SolrException | SolrServerException | IOException e) {
      LOGGER.error("Atomic update failed in solr for productSku : {} ", productSku, e);
    }
  }

  @Override
  public void deleteL3AndL4Documents(ProductDomainEventModel productDomainEventModel) {
    List<ProductSolr> productSolrList =
        productSolrRepository.findByProductCode(productDomainEventModel.getProductCode(),
            productDomainEventModel.isproductMarkForDelete());
    if (CollectionUtils.isNotEmpty(productSolrList)) {
      String productSku =
          Optional.ofNullable(productSolrList.stream().findFirst().get().getProductSku()).orElse(StringUtils.EMPTY);
      if (StringUtils.isNotEmpty(productSku)) {
        productSolrRepository.deleteSolrDocumentsByListOfProductSku(Collections.singleton(productSku));
        deleteSolrDocumentByProductSkuInL4Solr(productSku);
      }
    }
  }

  @Override
  public void reindexOnExternalSearch(ExternalSearchReindexToSolrEventModel externalSearchReindexToSolrEventModel) {
    Product product = productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        externalSearchReindexToSolrEventModel.getStoreId(), externalSearchReindexToSolrEventModel.getProductSku());
    if (Objects.nonNull(product)) {
      ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
      productAndItemEventModel.setProductSku(product.getProductSku());
      productAndItemEventModel.setProduct(objectConverter.convertToProductEventModel(product));
      productAndItemEventModel.setMerchantCode(product.getMerchantCode());
      if (autoHealOosProductsWithStock) {
        productAndItemEventModel.setSkipInventoryCallForAtomicUpdate(
            externalSearchReindexToSolrEventModel.isSkipInventoryCallForAtomicUpdate());
      }
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, externalSearchReindexToSolrEventModel.getProductSku(),
          productAndItemEventModel);
    }
  }
}
