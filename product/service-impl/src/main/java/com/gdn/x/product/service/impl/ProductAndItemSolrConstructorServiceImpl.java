package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;

@Service
public class ProductAndItemSolrConstructorServiceImpl
    implements ProductAndItemSolrConstructorService {

  private static final String ITEM_NOT_FOUND_FOR_ITEM_CODE =
      "#solrFailure item not found for item code : ";

  private static final String XPRODUCT_SOLR_REINDEX = "xproduct-solr-reindex";

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductAndItemSolrConstructorServiceImpl.class);

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Value("${sales.category.catalog.code}")
  private String salesCategoryCatalogCode;

  @Override
  public Map<String, FieldValueObject> constructByMasterDataChangeModel(ProductAndItemSolr productAndItemSolr,
      ProductDomainEventModel productDomainEventModel, Map<String, Double> productAndTotalScoreMap) {
    ProductAndItemSolrConstructorServiceImpl.LOGGER.debug("masterdata to be constructed: {}",
        productDomainEventModel);
    ProductAndItemSolr existingProductAndItemSolrData = new ProductAndItemSolr();
    BeanUtils.copyProperties(productAndItemSolr, existingProductAndItemSolrData);
    CategoryDomainEventModel masterCategory =
        productDomainEventModel.getProductCategories().get(0).getCategory();
    String catalogCode = masterCategory.getCatalog().getCatalogCode();
    if (StringUtils.isBlank(catalogCode)) {
      MasterCatalog masterCatalog =
          this.constructMasterCatalog(productAndItemSolr.getMasterCatalog());
      catalogCode = masterCatalog.getCatalogCode();
    }
    productAndItemSolr.setMasterCatalog(
        this.constructMasterCatalogString(catalogCode, masterCategory.getCategoryCode()));
    modifySalesCatalog(productAndItemSolr, productDomainEventModel);
    ProductItemDomainEventModel itemEventModel = null;
    for (ProductItemDomainEventModel model : productDomainEventModel.getProductItems()) {
        if (model.getSkuCode().equals(productAndItemSolr.getItemCode())) {
          itemEventModel = model;
          break;
        }
    }
    if (productAndItemSolr.isSynchronized()) {
      GdnPreconditions.checkArgument(itemEventModel != null,
          ProductAndItemSolrConstructorServiceImpl.ITEM_NOT_FOUND_FOR_ITEM_CODE
              + productAndItemSolr.getItemCode());
      productAndItemSolr.setBrand(productDomainEventModel.getBrand());
      productAndItemSolr.setProductName(productDomainEventModel.getName());
      productAndItemSolr.setItemName(
        Optional.ofNullable(itemEventModel).map(ProductItemDomainEventModel::getGeneratedItemName)
          .orElse(null));
    }
    List<String> itemImages = new ArrayList<>();
    if (Objects.nonNull(itemEventModel) && Objects.nonNull(itemEventModel.getImages())) {
      for (ImageDomainEventModel itemImage : itemEventModel.getImages()) {
        itemImages.add(
            this.constructItemImage(itemImage.isMainImage(), itemImage.getLocationPath(), itemImage.getSequence()));
      }
    }
    productAndItemSolr.setItemImages(itemImages);
    if (productAndTotalScoreMap.containsKey(productAndItemSolr.getProductSku())) {
      productAndItemSolr.setProductScoreTotal(
          Math.round(productAndTotalScoreMap.get(productAndItemSolr.getProductSku()) * Constants.ROUND_OFF_FACTOR)
              / Constants.ROUND_OFF_FACTOR);
    }
    Map<String, FieldValueObject> changedFieldsWithData =
        CommonUtil.checkIfSolrUpdateNeededForProductAndItemSolr(existingProductAndItemSolrData, productAndItemSolr);
    return changedFieldsWithData.size() > 0 ? changedFieldsWithData : null;
  }

  private void modifySalesCatalog(ProductAndItemSolr productAndItemSolr,
      ProductDomainEventModel productDomainEventModel) {
    if (Objects.nonNull(productDomainEventModel.getProductSalesCategoryMapping())) {
      List<String> oldSalesCatalog =
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getOldSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream().map(
              categoryCode -> new StringBuilder(salesCategoryCatalogCode).append(solrStringDelimiter)
                  .append(categoryCode).toString()).collect((Collectors.toList()));
      List<String> newSalesCatalog =
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getNewSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream().map(
              categoryCode -> new StringBuilder(salesCategoryCatalogCode).append(solrStringDelimiter)
                  .append(categoryCode).toString()).collect((Collectors.toList()));
      checkIfMerchantIsUmkmAndAddUmkmSalesCategory(productAndItemSolr, productDomainEventModel, newSalesCatalog);
      if (Objects.nonNull(productAndItemSolr.getSalesCatalog())) {
        productAndItemSolr.getSalesCatalog().removeAll(oldSalesCatalog);
        productAndItemSolr.getSalesCatalog().addAll(newSalesCatalog);
      } else {
        productAndItemSolr.setSalesCatalog(newSalesCatalog);
      }
    }
  }

  private void checkIfMerchantIsUmkmAndAddUmkmSalesCategory(ProductAndItemSolr productAndItemSolr,
      ProductDomainEventModel productDomainEventModel, List<String> newSalesCatalog) {
    if (CollectionUtils
        .isNotEmpty(productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes())
        && businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productAndItemSolr.getMerchantCode())) {
      newSalesCatalog.addAll(
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream().map(
              categoryCode -> new StringBuilder(salesCategoryCatalogCode).append(solrStringDelimiter)
                  .append(categoryCode).toString()).collect((Collectors.toList())));
    }
  }

  @Override
  public void constructItem(ProductAndItemSolr productItemSolr, Item item, boolean getMasterData) {
    productItemSolr.setId(item.getItemSku());
    productItemSolr.setStoreId(item.getStoreId());
    productItemSolr.setMarkForDelete(item.isMarkForDelete());
    List<String> buyable = new ArrayList<String>();
    List<String> discoverable = new ArrayList<String>();
    for (ItemViewConfig viewConfig : item.getItemViewConfigs()) {
      buyable.add(String.format("%s%s%s", viewConfig.getChannel(), this.solrStringDelimiter,
          viewConfig.isBuyable()));
      discoverable.add(String.format("%s%s%s", viewConfig.getChannel(), this.solrStringDelimiter,
          viewConfig.isDiscoverable()));
    }
    productItemSolr.setBuyable(buyable);
    productItemSolr.setDiscoverable(discoverable);
    productItemSolr.setItemCode(item.getItemCode());
    setWholesalePriceActivated(productItemSolr, item);
    try {
      String requestId = UUID.randomUUID().toString();
      boolean fetchOnlyItemImageDetails = Boolean.parseBoolean(systemParameterService
          .findValueByStoreIdAndVariable(item.getStoreId(), Constants.FETCH_ONLY_IMAGES_DETAILS).getValue());
      List<ImageResponse> imageResponses = new ArrayList<>();
      Item pcbItem = new Item();
      Map<String, List<ImageResponse>> itemCodesImageResponseMap = new HashMap<>();
      if (Objects.nonNull(item.getItemCode())) {
        if (fetchOnlyItemImageDetails) {
          SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
          skuCodesRequest.setSkuCodes(Collections.singletonList(item.getItemCode()));
          GdnRestListResponse<ItemImageResponse> productItemImagesByItemCode =
            productCategoryBaseOutbound.getProductItemImagesByItemCode(skuCodesRequest);
          itemCodesImageResponseMap =
            Optional.ofNullable(productItemImagesByItemCode.getContent()).orElse(new ArrayList<>())
              .stream().collect(Collectors.toMap(ItemImageResponse::getItemCode,
                ItemImageResponse::getImageResponses));
          pcbItem.setMasterDataItem(new MasterDataItem());
          pcbItem.getMasterDataItem().setGeneratedItemName(item.getGeneratedItemName());
        } else {
          pcbItem.setItemCode(item.getItemCode());
          pcbItem = this.productHelperService.setMasterDataItemFromMasterData(item.getStoreId(),
              requestId, ProductAndItemSolrConstructorServiceImpl.XPRODUCT_SOLR_REINDEX, pcbItem);
        }
      }

      if (item.isSynchronized() && getMasterData && Objects.nonNull(item.getItemCode())) {
        ProductAndItemSolrConstructorServiceImpl.LOGGER
            .info("call masterData at reindex solr process with requestId:" + requestId);
        productItemSolr.setItemName(pcbItem.getMasterDataItem().getGeneratedItemName());
      } else {
        productItemSolr.setItemName(item.getMasterDataItem().getGeneratedItemName());
      }
      List<String> itemImages = new ArrayList<String>();

      if (fetchOnlyItemImageDetails) {
        setItemImages(item, itemCodesImageResponseMap.get(item.getItemCode()), itemImages);
      } else {
        setItemImages(item, pcbItem, itemImages);
      }

      productItemSolr.setItemImages(itemImages);
    } catch (Exception e) {
      ProductAndItemSolrConstructorServiceImpl.LOGGER.warn("failed on getting item name {}", item,
          e);
    }
    productItemSolr.setItemSku(item.getItemSku());
    List<String> offerPrice = new ArrayList<String>();
    List<String> listPrice = new ArrayList<String>();
    productItemSolr.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
    for (Price price : item.getPrice()) {
      if(Objects.nonNull(price.getMerchantPromoDiscountPrice())){
        offerPrice.add(String.format("%s%s%.0f", price.getChannel(), this.solrStringDelimiter, price.getMerchantPromoDiscountPrice().getDiscountPrice()));
        productItemSolr.setMerchantPromoDiscountActivated(true);
      } else {
        offerPrice.add(String.format("%s%s%.0f", price.getChannel(), this.solrStringDelimiter, price.getOfferPrice()));
        productItemSolr.setMerchantPromoDiscountActivated(false);
      }
      listPrice.add(String.format("%s%s%.0f", price.getChannel(), this.solrStringDelimiter,
          price.getListPrice()));
    }

    constructOfflinePrices(productItemSolr);

    productItemSolr.setOfferPrice(offerPrice);
    productItemSolr.setListPrice(listPrice);
    productItemSolr.setMerchantSku(item.getMerchantSku());
    productItemSolr.setSynchronized(item.isSynchronized());
    productItemSolr.setLatefulfillment(item.isLateFulfillment());
    productItemSolr.setMarkForDelete(item.isMarkForDelete());
    productItemSolr.setPickupPointCode(item.getPickupPointCode());
    productItemSolr.setTicketTemplateCode(item.getTicketTemplateCode());
    productItemSolr.setCreatedDate(item.getCreatedDate());
    productItemSolr.setUpdatedDate(item.getUpdatedDate());
    productItemSolr.setOff2OnChannelActive(item.isOff2OnChannelActive());
    productItemSolr.setArchived(item.isArchived());
    productItemSolr.setPromoBundling(item.isPromoBundling());
    productItemSolr.setCncActivated(item.isCncActivated());
    productItemSolr.setFreeSample(item.isFreeSample());
    if (Objects.nonNull(item.getPristineDataItem())) {
      productItemSolr.setPristineId(item.getPristineDataItem().getPristineId());
    } else {
      productItemSolr.setPristineId(null);
    }
  }

  private void setItemImages(Item item, Item pcbItem, List<String> itemImages) {
    if (Objects.isNull(item.getItemCode())) {
      for (MasterDataItemImage masterDataItemImage : item.getMasterDataItem().getMasterDataItemImages()) {
        itemImages.add(this.constructItemImage(masterDataItemImage.isMainImage(), masterDataItemImage.getLocationPath(),
            masterDataItemImage.getSequence()));
      }
    } else {
      if (Objects.nonNull(pcbItem.getMasterDataItem().getMasterDataItemImages())) {
        for (MasterDataItemImage masterDataItemImage : pcbItem.getMasterDataItem().getMasterDataItemImages()) {
          itemImages.add(
              this.constructItemImage(masterDataItemImage.isMainImage(), masterDataItemImage.getLocationPath(),
                  masterDataItemImage.getSequence()));
        }
      }
    }
  }

  private void setItemImages(Item item, List<ImageResponse> imageResponses, List<String> itemImages) {
    if (Objects.isNull(item.getItemCode())) {
      item.getMasterDataItem().getMasterDataItemImages().forEach(masterDataItemImage ->
          itemImages.add(this.constructItemImage(masterDataItemImage.isMainImage(), masterDataItemImage.getLocationPath(),
              masterDataItemImage.getSequence())));
    } else {
      if (Objects.nonNull(imageResponses)) {
        imageResponses.forEach(imageResponse -> itemImages.add(
            constructItemImage(imageResponse.isMainImage(), imageResponse.getLocationPath(), imageResponse.getSequence())));
      }
    }
  }

  private void setWholesalePriceActivated(ProductAndItemSolr productItemSolr, Item item) {
    if (item.isWholesalePriceExists()) {
      if (Optional.ofNullable(item.getActivePromoBundlings()).orElse(new HashSet<>())
          .contains(Constants.WHOLESALE_PRICE)) {
        productItemSolr.setWholesalePriceActivated(true);
      } else {
        productItemSolr.setWholesalePriceActivated(false);
      }
    }
  }

  private String constructItemImage(boolean mainImage, String locationPath, int sequence) {
    return String.format("%s%s%s%s%s", mainImage, this.solrStringDelimiter, locationPath,
        this.solrStringDelimiter, sequence);
  }

  @Override
  public Set<ItemViewConfig> constructItemViewConfigs(List<String> buyables,
      List<String> discoverables) {
    Map<String, ItemViewConfig> itemViewConfigs = new HashMap<String, ItemViewConfig>();
    if (discoverables != null) {
      for (String discoverableString : discoverables) {
        String[] discoverable = discoverableString.split(this.solrStringDelimiter);
        String channel = discoverable[0];
        ItemViewConfig itemViewConfig = itemViewConfigs.get(channel);
        if (itemViewConfig == null) {
          itemViewConfig = new ItemViewConfig();
          itemViewConfig.setChannel(channel);
        }
        itemViewConfig.setDiscoverable(Boolean.valueOf(discoverable[1]));
        itemViewConfigs.put(channel, itemViewConfig);
      }
    }
    if (buyables != null) {
      for (String buyableString : buyables) {
        String[] buyable = buyableString.split(this.solrStringDelimiter);
        String channel = buyable[0];
        ItemViewConfig itemViewConfig = itemViewConfigs.get(channel);
        if (itemViewConfig == null) {
          itemViewConfig = new ItemViewConfig();
          itemViewConfig.setChannel(channel);
        }
        itemViewConfig.setBuyable(Boolean.valueOf(buyable[1]));
        itemViewConfigs.put(channel, itemViewConfig);
      }
    }
    return new HashSet<ItemViewConfig>(itemViewConfigs.values());
  }

  @Override
  public MasterCatalog constructMasterCatalog(String catalogString) {
    if (StringUtils.isBlank(catalogString)) {
      return null;
    }
    String masterCatalogAttr[] = catalogString.split(this.solrStringDelimiter);
    return new MasterCatalog(masterCatalogAttr[0],
        new Category(masterCatalogAttr[1], masterCatalogAttr[1]));
  }

  private String constructMasterCatalogString(String catalogCode, String categoryCode) {
    return String.format("%s%s%s", catalogCode, this.solrStringDelimiter, categoryCode);
  }

  @Override
  public List<MasterDataItemImage> constructMasterDataItemMainImages(List<String> imageString) {
    List<MasterDataItemImage> itemImages = new ArrayList<MasterDataItemImage>();
    if (imageString != null) {
      for (String image : imageString) {
        String imageAttr[] = image.split(this.solrStringDelimiter);
        MasterDataItemImage itemImage = new MasterDataItemImage(Boolean.valueOf(imageAttr[0]),
            imageAttr[1], Integer.valueOf(imageAttr[2]));
        if (itemImage.isMainImage()) {
          itemImages.add(itemImage);
        }
      }
    }
    return itemImages;
  }

  @Override
  public void constructProduct(ProductAndItemSolr productItemSolr, Product product,
      boolean getMasterData) {
    productItemSolr.setStoreId(product.getStoreId());
    productItemSolr.setMerchantCode(product.getMerchantCode());
    productItemSolr.setCurationStatus(CommonUtil.getCurationStatus(product));
    productItemSolr.setProductCode(product.getProductCode());
    productItemSolr.setProductSku(product.getProductSku());
    productItemSolr.setProductType(product.getProductType().toString());
    productItemSolr.setProductCatentryId(product.getProductCatentryId());
    if (Objects.nonNull(product.getProductScore())) {
      productItemSolr.setProductScoreTotal(
          Math.round(product.getProductScore().getTotalScore() * Constants.ROUND_OFF_FACTOR)
              / Constants.ROUND_OFF_FACTOR);
    }
    List<String> salesCatalogs = new ArrayList<String>();
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      for (Category category : salesCatalog.getListOfCategories()) {
        salesCatalogs.add(String.format("%s%s%s", salesCatalog.getCatalogCode(),
            this.solrStringDelimiter, category.getCategoryCode()));
      }
    }
    productItemSolr.setSalesCatalog(salesCatalogs);
    productItemSolr.setTradingProduct(product.isTradingProduct());
    productItemSolr.setSuspended(product.isSuspended());
    productItemSolr.setMarkForDelete(product.isMarkForDelete());
    productItemSolr.setFreeSample(product.isFreeSample());
    if(Objects.nonNull(product.getPreOrder()) && Objects.nonNull(product.getPreOrder().getIsPreOrder())) {
      productItemSolr.setPreOrderActive(product.getPreOrder().getIsPreOrder());
    }
    try {
      MasterDataProduct oldMasterData = product.getMasterDataProduct();
      if (getMasterData) {
        String requestId = UUID.randomUUID().toString();
        ProductAndItemSolrConstructorServiceImpl.LOGGER
            .info("call masterData at reindex solr process with requestId:" + requestId);
        product = this.productHelperService.setMasterDataProductFromMasterData(product.getStoreId(),
            requestId, ProductAndItemSolrConstructorServiceImpl.XPRODUCT_SOLR_REINDEX, product);
      }
      MasterCatalog masterCatalog = null;
      if (StringUtils.isBlank(product.getProductCode())) {
        masterCatalog = product.getMasterCatalog();
      } else {
        masterCatalog = product.getMasterDataProduct().getMasterCatalog();
      }

      if(Objects.nonNull(masterCatalog) && Objects.nonNull(masterCatalog.getCategory())) {
        productItemSolr.setMasterCatalog(this.constructMasterCatalogString(masterCatalog.getCatalogCode(),
            masterCatalog.getCategory().getCategoryCode()));
      }
      MasterDataProduct selectedMasterData;
      if (product.isSynchronized()) {
        selectedMasterData = product.getMasterDataProduct();
      } else {
        selectedMasterData = oldMasterData;
      }
      productItemSolr.setProductName(selectedMasterData.getProductName());
      productItemSolr.setBrand(selectedMasterData.getBrand());
      product.setMasterDataProduct(selectedMasterData);
    } catch (Exception e) {
      ProductAndItemSolrConstructorServiceImpl.LOGGER.warn("failed on getting product name {}",
          product, e);
    }
  }

  @Override
  public List<SalesCatalog> constructSalesCatalogs(List<String> catalogString) {
    List<SalesCatalog> salesCatalogs = new ArrayList<SalesCatalog>();
    Map<String, List<Category>> mapOfSalesCategories = new HashMap<String, List<Category>>();
    if (catalogString == null) {
      return salesCatalogs;
    }
    for (String salesCatalog : catalogString) {
      String salesCatalogValues[] = salesCatalog.split(this.solrStringDelimiter);
      String catalogCode = salesCatalogValues[0];
      String categoryCode = salesCatalogValues[1];
      List<Category> categories = mapOfSalesCategories.get(catalogCode);
      if (categories == null) {
        categories = new ArrayList<Category>();
        mapOfSalesCategories.put(catalogCode, categories);
      }
      Category category = new Category(categoryCode, categoryCode);
      categories.add(category);
    }
    for (Entry<String, List<Category>> entry : mapOfSalesCategories.entrySet()) {
      salesCatalogs.add(new SalesCatalog(entry.getKey(), entry.getValue()));
    }
    return salesCatalogs;
  }

  public void setSolrStringDelimiter(String solrStringDelimiter) {
    this.solrStringDelimiter = solrStringDelimiter;
  }

  @Override
  public void constructOfflinePrices(ProductAndItemSolr productItemSolr) {
    List<String> offlinePrices = getOfflinePrices(productItemSolr.getStoreId(), productItemSolr.getItemSku());
    productItemSolr.setOfflinePrices(offlinePrices);
  }

  private List<String> getOfflinePrices(String storeId, String itemSku) {
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, itemSku, true, false);
    List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
    List<String> offlinePrices = new ArrayList<>();
    if (!CollectionUtils.isEmpty(offlineItems)) {
      offlineItems.forEach(offlineItem -> {
        Double offerPrice = offlineItem.getOfferPrice();
        String offlinePrice = offlineItem.getPickupPointCode() + this.solrStringDelimiter
            + Optional.ofNullable(offlineItem.getListPrice()).orElse(offerPrice)
            + this.solrStringDelimiter + offerPrice;
        offlinePrices.add(offlinePrice);
      });
    }
    return offlinePrices;
  }

  @Override
  public List<MasterDataItemImage> constructMasterDataItemImages(List<String> imageString) {
    List<MasterDataItemImage> itemImages = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(imageString)) {
      for (String image : imageString) {
        String imageAttr[] = image.split(this.solrStringDelimiter);
        MasterDataItemImage itemImage =
            new MasterDataItemImage(Boolean.valueOf(imageAttr[0]), imageAttr[1], Integer.valueOf(imageAttr[2]));
        itemImages.add(itemImage);
      }
    }
    return itemImages;
  }

  @Override
  public List<String> getOfflinePricesByStoreIdAndItemSku(String storeId, String itemSku) {
    return getOfflinePrices(storeId, itemSku);
  }
}
