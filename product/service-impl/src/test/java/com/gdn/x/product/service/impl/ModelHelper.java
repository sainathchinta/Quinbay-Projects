package com.gdn.x.product.service.impl;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.product.enums.InventoryType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ModelHelper {
  public static final boolean BUYABLE = true;
  public static final String CHANNEL = "CHANNEL";
  public static final String LOCATION_PATH = "locationPath";
  public static final boolean IS_MAIN_IMAGE = true;
  public static final boolean DISCOVERABLE = true;
  public static final String CATEGORY_CODE = "categoryCode";
  public static final Integer SEQUENCE = 0;
  public static final byte[] BYTES = "byte".getBytes();
  public static final String ITEM_SKU = "itemSku";
  public static final String MERCHANT_SKU = "merchantSku";
  public static final String ITEM_CODE = "itemCode";
  public static final String ITEM_NAME = "itemName";
  public static final String PRODUCT_SKU = "productSku";
  public static final String PRODUCT_CODE = "productCode";
  public static final String PRODUCT_TYPE = "productType";
  public static final String PRODUCT_NAME = "productName";
  public static final String MERCHANT_CODE = "merchantCode";
  public static final boolean IS_SYNCHRONIZED = false;
  public static final String MASTER_CATALOG = "masterCatalog";
  public static final boolean IS_LATEFULFILLMENT = false;
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  public static final String BRAND = "brand";
  public static final String PRODUCT_CATENTRY_ID = "productCatentryId";
  public static final String TICKET_TEMPLATE_CODE = "ticketTemplateCode";
  public static final String NAME = "name";
  public static final Double LENGTH = 0.0;
  public static final Double WIDTH = 0.0;
  public static final Double WEIGHT = 0.0;
  public static final Double HEIGHT = 0.0;
  public static final Double SHIPPING_WEIGHT = 0.0;
  public static final String SPECIFICATION_DETAIL = "specificationDetail";
  public static final String PRODUCT_STORY = "productStory";
  public static final String UNIQUE_SELLING_POINT = "uniqueSellingPoint";
  public static final String UOM = "uom";
  public static final boolean ACTIVATED = false;
  public static final boolean VIEWABLE = false;
  public static final boolean ARCHIVED = false;
  public static final String URL = "url";
  public static final boolean DISPLAY = false;
  public static final Integer LOGISTIC_ADJUSTMENT = 0;
  public static final boolean WARRANTY = false;
  public static final boolean NEED_IDENTITY = false;
  public static final String PARENT_CATEGORY_ID = "parentCategoryId";
  public static final String CATALOG_CODE = "catalogCode";
  public static final String CATALOG_TYPE = "catalogType";
  public static final String ITEM_CATENTRY_ID = "itemCatentryId";
  public static final Boolean IS_LATE_FULFILLMENT = false;
  public static final String ETD_NOTE = "etdNote";
  public static final String SKU_CODE = "skuCode";
  public static final String GENERATED_ITEM_NAME = "generatedItemName";
  public static final String UPC_CODE = "upcCode";
  public static final boolean IS_ACTIVATED = false;
  public static final boolean IS_VIEWABLE = false;
  public static final String HASH = "hash";
  public static final int DANGEROUS_LEVEL = 0;
  public static final Double ITEM_DELIVERY_WEIGHT = 0.0;
  public static final Double ITEM_HEIGHT = 0.0;
  public static final Double ITEM_LENGTH = 0.0;
  public static final Double ITEM_WEIGHT = 0.0;
  public static final Double ITEM_WIDTH = 0.0;
  public static final InventoryType INVENTORY_TYPE = InventoryType.WITH_INVENTORY;
  public static final String LONG_DESCRIPTION = "longDescription";
  public static final String SETTLEMENT_TYPE = "settlementType";
  public static final boolean INSTALLATION_REQUIRED = false;
  public static final String DESCRIPTION = "description";
  public static final int CODE = 0;
  public static final String CATGROUP_ID = "catGroupId";
  private static final String STORE_ID = "10001";

  public Item setupItem() {
    Item item = new Item();
    item.setItemSku(ModelHelper.ITEM_SKU);
    item.setProductSku(ModelHelper.PRODUCT_SKU);
    item.setMerchantSku(ModelHelper.MERCHANT_SKU);
    item.setItemCode(ModelHelper.ITEM_CODE);
    item.setSynchronized(ModelHelper.IS_SYNCHRONIZED);
    item.setItemCatentryId(ModelHelper.ITEM_CATENTRY_ID);
    List<MasterDataItemImage> masterDataItemImages = new ArrayList<MasterDataItemImage>();
    masterDataItemImages.add(new MasterDataItemImage());
    HashSet<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();
    itemViewConfigs.add(new ItemViewConfig(ModelHelper.BUYABLE, ModelHelper.DISCOVERABLE,
        ModelHelper.CHANNEL, null, null));
    item.setItemViewConfigs(itemViewConfigs);
    Set<Price> prices = new HashSet<Price>();
    Price price = new Price();
    price.setOfferPrice(200);
    price.setChannel(CHANNEL);
    price.setMerchantPromoDiscountPrice(new DiscountPrice(400, new Date(), new Date(), "",
        AdjustmentType.MERCHANT));
    prices.add(price);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setSkuCode(ModelHelper.SKU_CODE);
    masterDataItem.setGeneratedItemName(ModelHelper.GENERATED_ITEM_NAME);
    masterDataItem.setUpcCode(ModelHelper.UPC_CODE);
    masterDataItem.setActivated(ModelHelper.IS_ACTIVATED);
    masterDataItem.setViewable(ModelHelper.IS_VIEWABLE);
    masterDataItem.setHash(ModelHelper.HASH);
    masterDataItem.setInventoryType(ModelHelper.INVENTORY_TYPE);
    masterDataItem.setDangerousLevel(ModelHelper.DANGEROUS_LEVEL);
    masterDataItem.setItemDeliveryWeight(ModelHelper.ITEM_DELIVERY_WEIGHT);
    masterDataItem.setItemHeight(ModelHelper.ITEM_HEIGHT);
    masterDataItem.setItemLength(ModelHelper.ITEM_LENGTH);
    masterDataItem.setItemWeight(ModelHelper.ITEM_WEIGHT);
    masterDataItem.setItemWidth(ModelHelper.ITEM_WIDTH);
    masterDataItem.setMasterDataItemImages(masterDataItemImages);
    masterDataItem.setProductCode(ModelHelper.PRODUCT_CODE);
    item.setMasterDataItem(masterDataItem);
    item.setLateFulfillment(ModelHelper.IS_LATE_FULFILLMENT);
    item.setPickupPointCode(ModelHelper.PICKUP_POINT_CODE);
    item.setTicketTemplateCode(ModelHelper.TICKET_TEMPLATE_CODE);
    item.setPrice(prices);
    return item;
  }

  public Product setupProduct() {
    List<ProductAttribute> definingAttributes = new ArrayList<ProductAttribute>();
    List<ProductAttributeDetail> descriptiveAttributes = new ArrayList<ProductAttributeDetail>();
    List<ProductSpecialAttribute> productSpecialAttributes =
        new ArrayList<ProductSpecialAttribute>();
    List<SalesCatalog> salesCatalogs = new ArrayList<SalesCatalog>();
    List<MasterDataProductImage> masterDataProductImages = new ArrayList<MasterDataProductImage>();
    List<MasterDataProductAttribute> masterDataProductAttributes =
        new ArrayList<MasterDataProductAttribute>();
    List<SalesCategorySequence> salesCategorySequences = new ArrayList<SalesCategorySequence>();
    List<ItemCatalogVO> itemCatalogs = new ArrayList<ItemCatalogVO>();

    salesCatalogs.add(new SalesCatalog());

    Product product = new Product();
    product.setProductSku(ModelHelper.PRODUCT_SKU);
    product.setProductCode(ModelHelper.PRODUCT_CODE);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setSettlementType(ModelHelper.SETTLEMENT_TYPE);
    product.setMerchantCode(ModelHelper.MERCHANT_CODE);
    product.setSynchronized(ModelHelper.IS_SYNCHRONIZED);
    product.setProductCatentryId(ModelHelper.PRODUCT_CATENTRY_ID);
    product.setDefiningAttributes(definingAttributes);
    product.setDescriptiveAttributes(descriptiveAttributes);
    product.setProductSpecialAttributes(productSpecialAttributes);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(ModelHelper.CATALOG_CODE);
    Category category = new Category();
    category.setCategoryCode(ModelHelper.CATEGORY_CODE);
    category.setCatgroupId(ModelHelper.CATGROUP_ID);
    masterCatalog.setCategory(category);
    product.setMasterCatalog(masterCatalog);
    product.setSalesCatalogs(salesCatalogs);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrand(ModelHelper.BRAND);
    masterDataProduct.setShippingWeight(ModelHelper.SHIPPING_WEIGHT);
    masterDataProduct.setSpecificationDetail(ModelHelper.SPECIFICATION_DETAIL);
    masterDataProduct.setProductName(ModelHelper.PRODUCT_NAME);
    masterDataProduct.setDescription(ModelHelper.DESCRIPTION);
    masterDataProduct.setLongDescription(ModelHelper.LONG_DESCRIPTION);
    masterDataProduct.setUniqueSellingPoint(ModelHelper.UNIQUE_SELLING_POINT);
    masterDataProduct.setActivated(ModelHelper.IS_ACTIVATED);
    masterDataProduct.setViewable(ModelHelper.IS_VIEWABLE);
    masterDataProduct.setProductStory(ModelHelper.PRODUCT_STORY);
    masterDataProduct.setUom(ModelHelper.UOM);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
    masterDataProduct.setUrl(ModelHelper.URL);
    masterDataProduct.setLength(ModelHelper.LENGTH);
    masterDataProduct.setWidth(ModelHelper.WIDTH);
    masterDataProduct.setHeight(ModelHelper.HEIGHT);
    masterDataProduct.setWeight(ModelHelper.WEIGHT);
    masterCatalog.setCatalogCode(ModelHelper.CATALOG_CODE);
    product.setMasterDataProduct(masterDataProduct);
    product.setSalesCategorySequences(salesCategorySequences);
    product.setItemCatalogs(itemCatalogs);
    product.setInstallationRequired(ModelHelper.INSTALLATION_REQUIRED);
    return product;
  }

  public ProductAndItemSolr setupProductAndItemSolr() {
    List<String> offerPrice = new ArrayList<String>();
    List<String> listPrice = new ArrayList<String>();
    List<String> buyable = new ArrayList<String>();
    List<String> discoverable = new ArrayList<String>();
    List<String> salesCatalog = new ArrayList<String>();
    List<String> itemImages = new ArrayList<String>();
    Date createdDate = new Date();
    Date updatedDate = new Date();

    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ModelHelper.ITEM_SKU);
    productAndItemSolr.setMerchantSku(ModelHelper.MERCHANT_SKU);
    productAndItemSolr.setItemCode(ModelHelper.ITEM_CODE);
    productAndItemSolr.setItemName(ModelHelper.ITEM_NAME);
    productAndItemSolr.setMerchantCode(MERCHANT_CODE);
    productAndItemSolr.setOfferPrice(offerPrice);
    productAndItemSolr.setListPrice(listPrice);
    productAndItemSolr.setBuyable(buyable);
    productAndItemSolr.setDiscoverable(discoverable);
    productAndItemSolr.setProductSku(ModelHelper.PRODUCT_SKU);
    productAndItemSolr.setProductCode(ModelHelper.PRODUCT_CODE);
    productAndItemSolr.setProductType(ModelHelper.PRODUCT_TYPE);
    productAndItemSolr.setProductName(ModelHelper.PRODUCT_NAME);
    productAndItemSolr.setMerchantCode(ModelHelper.MERCHANT_CODE);
    productAndItemSolr.setSynchronized(ModelHelper.IS_SYNCHRONIZED);
    productAndItemSolr.setMasterCatalog(ModelHelper.MASTER_CATALOG);
    productAndItemSolr.setSalesCatalog(salesCatalog);
    productAndItemSolr.setLatefulfillment(ModelHelper.IS_LATEFULFILLMENT);
    productAndItemSolr.setPickupPointCode(ModelHelper.PICKUP_POINT_CODE);
    productAndItemSolr.setBrand(ModelHelper.BRAND);
    productAndItemSolr.setItemImages(itemImages);
    productAndItemSolr.setProductCatentryId(ModelHelper.PRODUCT_CATENTRY_ID);
    productAndItemSolr.setTicketTemplateCode(ModelHelper.TICKET_TEMPLATE_CODE);
    productAndItemSolr.setCreatedDate(createdDate);
    productAndItemSolr.setUpdatedDate(updatedDate);
    productAndItemSolr.setArchived(ModelHelper.ARCHIVED);
    productAndItemSolr.setStoreId(ModelHelper.STORE_ID);

    return productAndItemSolr;
  }

  private CategoryDomainEventModel setupProductCategoryDomainEventModel() {
    CategoryDomainEventModel category = new CategoryDomainEventModel();
    category.setName(ModelHelper.NAME);
    category.setCategoryCode(ModelHelper.CATEGORY_CODE);
    category.setSequence(ModelHelper.SEQUENCE);
    byte[] description = ModelHelper.BYTES;
    category.setDescription(description);
    category.setDisplay(ModelHelper.DISPLAY);
    category.setLogisticAdjustment(ModelHelper.LOGISTIC_ADJUSTMENT);
    category.setWarranty(ModelHelper.WARRANTY);
    category.setNeedIdentity(ModelHelper.NEED_IDENTITY);
    category.setActivated(ModelHelper.ACTIVATED);
    category.setViewable(ModelHelper.VIEWABLE);
    CatalogDomainEventModel catalog = new CatalogDomainEventModel();
    catalog.setName(ModelHelper.NAME);
    catalog.setCatalogCode(ModelHelper.CATALOG_CODE);
    catalog.setCatalogType(ModelHelper.CATALOG_TYPE);
    category.setCatalog(catalog);
    category.setParentCategoryId(ModelHelper.PARENT_CATEGORY_ID);
    return category;
  }

  public ProductDomainEventModel setupProductDomainEventModel() {
    ProductItemDomainEventModel domainEventModel = new ProductItemDomainEventModel();
    domainEventModel.setSkuCode(ModelHelper.ITEM_CODE);
    ArrayList<ImageDomainEventModel> images = new ArrayList<ImageDomainEventModel>();
    images.add(new ImageDomainEventModel(ModelHelper.IS_MAIN_IMAGE, ModelHelper.LOCATION_PATH,
        ModelHelper.SEQUENCE));
    domainEventModel.setImages(images);
    List<ProductItemDomainEventModel> productItems = new ArrayList<ProductItemDomainEventModel>();
    productItems.add(domainEventModel);
    List<ProductCategoryDomainEventModel> productCategories =
        new ArrayList<ProductCategoryDomainEventModel>();
    List<ProductAttributeDomainEventModel> productAttributes =
        new ArrayList<ProductAttributeDomainEventModel>();

    ProductCategoryDomainEventModel productCategoryDomainEventModel =
        new ProductCategoryDomainEventModel();
    productCategoryDomainEventModel.setCategory(this.setupProductCategoryDomainEventModel());
    productCategories.add(productCategoryDomainEventModel);

    ProductDomainEventModel productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setName(ModelHelper.NAME);
    productDomainEventModel.setProductCode(ModelHelper.PRODUCT_CODE);
    productDomainEventModel.setLength(ModelHelper.LENGTH);
    productDomainEventModel.setWidth(ModelHelper.WIDTH);
    productDomainEventModel.setWeight(ModelHelper.WEIGHT);
    productDomainEventModel.setHeight(ModelHelper.HEIGHT);
    productDomainEventModel.setShippingWeight(ModelHelper.SHIPPING_WEIGHT);
    productDomainEventModel.setDescription(ModelHelper.BYTES);
    productDomainEventModel.setSpecificationDetail(ModelHelper.SPECIFICATION_DETAIL);
    productDomainEventModel.setProductStory(ModelHelper.PRODUCT_STORY);
    productDomainEventModel.setBrand(ModelHelper.BRAND);
    productDomainEventModel.setUniqueSellingPoint(ModelHelper.UNIQUE_SELLING_POINT);
    productDomainEventModel.setUom(ModelHelper.UOM);
    productDomainEventModel.setActivated(ModelHelper.ACTIVATED);
    productDomainEventModel.setViewable(ModelHelper.VIEWABLE);
    productDomainEventModel.setUrl(ModelHelper.URL);
    productDomainEventModel.setProductItems(productItems);
    productDomainEventModel.setProductCategories(productCategories);
    productDomainEventModel.setProductAttributes(productAttributes);
    productDomainEventModel.setImages(images);
    productDomainEventModel.setStoreId(ModelHelper.STORE_ID);
    return productDomainEventModel;
  }

  public ItemPickupPoint setupItemPickupPoint() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ModelHelper.ITEM_SKU);
    itemPickupPoint.setProductSku(ModelHelper.PRODUCT_SKU);
    itemPickupPoint.setMerchantSku(ModelHelper.MERCHANT_SKU);
    HashSet<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs
        .add(new ItemViewConfig(ModelHelper.BUYABLE, ModelHelper.DISCOVERABLE, ModelHelper.CHANNEL, null, null));
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Set<Price> prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(200);
    price.setChannel(CHANNEL);
    price.setMerchantPromoDiscountPrice(
        new DiscountPrice(400, new Date(), new Date(), StringUtils.EMPTY, AdjustmentType.MERCHANT));
    prices.add(price);
    itemPickupPoint.setPickupPointCode(ModelHelper.PICKUP_POINT_CODE);
    itemPickupPoint.setPrice(prices);
    return itemPickupPoint;
  }

}
