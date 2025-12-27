package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryResponseV2DTO;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;

@Component
public class ProductLevel3ConverterBean implements ProductLevel3Converter {

  @Override
  public ProductLevel3Inventory convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
      InventoryDetailInfoResponseV2DTO source) {
    if (source == null) {
      return null;
    }
    ProductLevel3Inventory inv = new ProductLevel3Inventory();
    WebInventoryResponseV2DTO webInv = source.getWebInventoryResponse();
    if (webInv != null) {
      inv.setWebItemSku(webInv.getWebItemSku());
      inv.setWebMerchantCode(webInv.getWebMerchantCode());
      inv.setWarehouseItemSku(webInv.getWarehouseItemSku());
      inv.setWarehouseMerchantCode(webInv.getWarehouseMerchantCode());
      inv.setWebAvailable(webInv.getAvailableStock());
      inv.setInitialPreOrderQuota(webInv.getInitialPreorderQuota());
      inv.setActualAvailableStock(webInv.getActualAvailableStock());
      inv.setWebReserved(webInv.getOriginalStock() - webInv.getAvailableStock());
      inv.setWebMinAlert(webInv.getMinimumStockAlert());
      inv.setWebSyncStock(webInv.isSyncStock());
      inv.setWebPickupPointCode(webInv.getPickupPointCode());
    }
    List<WarehouseInventoryResponseDTO> warehouseInvs = source.getWarehouseInventoryResponseList();
    if (warehouseInvs != null) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : warehouseInvs) {
        if (warehouseInv != null) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
      }
      inv.setWarehouseAvailable(warehouseAvailable);
      inv.setWarehouseReserved(warehouseOrigin - warehouseAvailable);
    }

    List<WarehouseInventoryResponseDTO> nonDistWarehouseInvs = source.getNonDistributionWarehouseInventoryResponseList();
    if (nonDistWarehouseInvs != null) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : nonDistWarehouseInvs) {
        if (warehouseInv != null) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
      }
      inv.setNonDistributionAvailable(warehouseAvailable);
      inv.setNonDistributionReserved(warehouseOrigin - warehouseAvailable);
    }

    return inv;
  }

  @Override
  public ProductLevel3Inventory convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(
      InventoryDetailInfoResponseDTO source) {
    if (source == null) {
      return null;
    }
    ProductLevel3Inventory inv = new ProductLevel3Inventory();
    WebInventoryResponseDTO webInv = source.getWebInventoryResponse();
    if (webInv != null) {
      inv.setWebItemSku(webInv.getWebItemSku());
      inv.setWebMerchantCode(webInv.getWebMerchantCode());
      inv.setWarehouseItemSku(webInv.getWarehouseItemSku());
      inv.setWarehouseMerchantCode(webInv.getWarehouseMerchantCode());
      inv.setWebAvailable(webInv.getAvailableStock());
      inv.setWebReserved(webInv.getOriginalStock() - webInv.getAvailableStock());
      inv.setWebMinAlert(webInv.getMinimumStockAlert());
      inv.setWebSyncStock(webInv.isSyncStock());
      inv.setWebPickupPointCode(webInv.getPickupPointCode());
    }
    List<WarehouseInventoryResponseDTO> warehouseInvs = source.getWarehouseInventoryResponseList();
    if (warehouseInvs != null) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : warehouseInvs) {
        if (warehouseInv != null) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
      }
      inv.setWarehouseAvailable(warehouseAvailable);
      inv.setWarehouseReserved(warehouseOrigin - warehouseAvailable);
    }

    List<WarehouseInventoryResponseDTO> nonDistWarehouseInvs = source.getNonDistributionWarehouseInventoryResponseList();
    if (nonDistWarehouseInvs != null) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : nonDistWarehouseInvs) {
        if (warehouseInv != null) {
          warehouseAvailable += warehouseInv.getAvailableStock();
          warehouseOrigin += warehouseInv.getOriginalStock();
        }
      }
      inv.setNonDistributionAvailable(warehouseAvailable);
      inv.setNonDistributionReserved(warehouseOrigin - warehouseAvailable);
    }

    return inv;
  }

  @Override
  public List<String> convertItemSummaryResponseToListOfGdnSku(List<ItemSummaryResponse> source) {
    if (source == null) {
      return new ArrayList<>();
    }
    return source.stream().map(item -> item.getItemSku()).collect(Collectors.toList());
  }

  @Override
  public Map<String, String> convertItemSummaryResponseToItemSkuAndPickupCodeMap(List<ItemSummaryResponse> source) {
    if (source.isEmpty()) {
      return new HashMap<>();
    }
    return source.stream().collect(
        Collectors.toMap(ItemSummaryResponse::getItemSku, ItemSummaryResponse::getPickupPointCode, (a, b) -> a));
  }

  @Override
  public List<String> convertProductLevel3InventoryToListOfGdnSku(
      List<ProductLevel3Inventory> source) {
    if (source == null) {
      return new ArrayList<>();
    }
    return source.stream().map(s -> s.getWebItemSku()).collect(Collectors.toList());
  }

  @Override
  public Map<String, ProductLevel3Inventory> convertProductLevel3InventoryToMapOfGdnSku(
      List<ProductLevel3Inventory> source) {
    return this.convertProductLevel3InventoryToMapOfGdnSku(source, null);
  }

  @Override
  public Map<String, ProductLevel3Inventory> convertProductLevel3InventoryToMapOfGdnSku(
      List<ProductLevel3Inventory> source, List<String> filters) {
    if (source == null) {
      return new HashMap<>();
    }
    if (CollectionUtils.isEmpty(filters)) {
      return source.stream().collect(Collectors.toMap(inv -> inv.getWebItemSku(), inv -> inv));
    } else {
      return source.stream().filter(inv -> filters.contains(inv.getWebItemSku()))
          .collect(Collectors.toMap(inv -> inv.getWebItemSku(), inv -> inv));
    }
  }

  @Override
  public ItemSummaryRequest convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
      ProductLevel3SummaryFilter filterRequest) {
    List<String> itemSkus = null;
    if (StringUtils.isNotBlank(filterRequest.getGdnSku()) || CollectionUtils.isNotEmpty(filterRequest.getItemSkus())) {
      itemSkus = new ArrayList<>();
      CollectionUtils.addIgnoreNull(itemSkus, filterRequest.getGdnSku());
      if (CollectionUtils.isNotEmpty(filterRequest.getItemSkus())) {
        itemSkus.addAll(filterRequest.getItemSkus());
      }
    }
    ItemSummaryRequest request = new ItemSummaryRequest(filterRequest.getBusinessPartnerCode(),
      filterRequest.getItemName(), itemSkus, null, filterRequest.getCategoryCode(), filterRequest.getSalePrice(),
      filterRequest.getPickupPointCode(), filterRequest.getDisplayable(), filterRequest.getBuyable(), null,
      filterRequest.getItemCode());
    request.setArchived(filterRequest.getArchived());
    request.setMerchantSkus(filterRequest.getMerchantSkus());
    request.setExcludedItemSkus(filterRequest.getExcludedItemSkus());
    request.setLinkedPartnerCode(filterRequest.getLinkedPartnerCode());
    request.setCategoryCodes(filterRequest.getCategoryCodes());
    request.setSearchKey(filterRequest.getSearchKey());
    request.setPickupPointCodes(filterRequest.getPickupPointCodes());
    request.setProductSkus(filterRequest.getProductSkuList());
    return request;
  }

  @Override
  public List<ProductLevel3Price> convertItemPricesToProductLevel3Prices(
      Collection<PriceDTO> itemPrices) {
    List<ProductLevel3Price> result = new ArrayList<>();
    for (PriceDTO priceData : itemPrices) {
      ProductLevel3Price price = new ProductLevel3Price();
      price.setChannelId(priceData.getChannel());
      price.setPrice(priceData.getListPrice());
      price.setSalePrice(priceData.getOfferPrice());
      if (CollectionUtils.isNotEmpty(priceData.getListOfDiscountPrices())) {
        price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
        price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
        price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
        price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
      }
      result.add(price);
    }
    return result;
  }

  @Override
  public List<ProductLevel3ViewConfig> convertItemViewConfigsToProductLevel3ViewConfigs(
      Collection<ItemViewConfigDTO> itemViewConfigs) {
    List<ProductLevel3ViewConfig> result = new ArrayList<>();
    for (ItemViewConfigDTO viewConfigData : itemViewConfigs) {
      ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
      viewConfig.setChannelId(viewConfigData.getChannel());
      viewConfig.setDisplay(viewConfigData.isDiscoverable());
      viewConfig.setBuyable(viewConfigData.isBuyable());
      result.add(viewConfig);
    }
    return result;
  }

  @Override
  public List<ProductLevel3Image> convertMasterDataItemImagesToProductLevel3Images(
      Collection<MasterDataItemImageDTO> itemImages) {
    List<ProductLevel3Image> result = new ArrayList<>();
    for (MasterDataItemImageDTO imageData : itemImages) {
      ProductLevel3Image image = new ProductLevel3Image();
      image.setMainImage(imageData.isMainImage());
      image.setSequence(imageData.getSequence());
      image.setLocationPath(imageData.getLocationPath());
      result.add(image);
    }
    return result;
  }

  @Override
  public ItemSummaryRequest convertProductLevel3ItemSearchToItemSummaryRequest(
      ProductLevel3ItemSearch search) {
    if (search == null) {
      return new ItemSummaryRequest();
    }
    String merchantCode = search.getBusinessPartnerCode();
    Boolean isTradingProduct = null;
    if (search.isIncludeAllTradingProduct()) {
      merchantCode = null;
      isTradingProduct = Boolean.TRUE;
    }

    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setMerchantCode(merchantCode);
    itemSummaryRequest.setProductItemName(search.getItemNameKeyword());
    itemSummaryRequest.setItemSkuKeyword(search.getItemSkuKeyword());
    itemSummaryRequest.setBuyable(search.getBuyable());
    itemSummaryRequest.setArchived(search.getIsArchived());
    itemSummaryRequest.setIsTradingProduct(isTradingProduct);
    return itemSummaryRequest;
  }

  @Override
  public ProductLevel3Item convertItemSummaryResponseToProductLevel3Item(
      ItemSummaryResponse source) {
    return ProductLevel3Item.builder().businessPartnerCode(source.getMerchantCode())
        .productCode(source.getProductCode()).productSku(source.getProductSku())
        .itemSku(source.getItemSku()).itemCode(source.getItemCode())
        .merchantSku(source.getMerchantSku()).itemName(source.getGeneratedItemName())
        .merchantPromoDiscount(source.isMerchantPromoDiscount())
        .merchantPromoDiscountActivated(source.isMerchantPromoDiscountActivated())
        .build();
  }

  @Override
  public List<ProductLevel3Logistics> convertLogisticDetailsToItemLevel3Logistics(
      List<GetSkuLogisticProductResponse> getSkuLogisticProductResponses) {
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    for (GetSkuLogisticProductResponse getSkuLogisticProductResponse : getSkuLogisticProductResponses) {
      ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
      productLevel3Logistics
          .setLogisticProductCode(getSkuLogisticProductResponse.getLogisticProductCode());
      productLevel3Logistics
          .setLogisticProductName(getSkuLogisticProductResponse.getLogisticProductName());
      productLevel3Logistics.setSelected(getSkuLogisticProductResponse.isSelected());
      productLevel3Logistics.setRequiredLongLat(getSkuLogisticProductResponse.isRequiredLongLat());
      productLevel3Logistics
          .setHighlightedInformation(getSkuLogisticProductResponse.getHighlightedInformation());
      productLevel3LogisticsList.add(productLevel3Logistics);
    }
    return productLevel3LogisticsList;
  }

  @Override
  public ItemsSummaryDetailRequest convertProductLevel3SummaryDetailsRequestToItemSummaryRequest(
      ProductLevel3SummaryFilterDetails filterRequest) {
    ItemsSummaryDetailRequest request = new ItemsSummaryDetailRequest();
    if (StringUtils.isNotEmpty(filterRequest.getProductSku())) {
      request.setProductSku(filterRequest.getProductSku());
    }
    if (StringUtils.isNotEmpty(filterRequest.getItemSku())) {
      request.setItemSku(filterRequest.getItemSku());
    }
    return request;
  }

  @Override
  public ProductLevel3SummaryDetailsImage convertMasterDataItemImagesToProductLevel3SummaryDetailsImage(
      MasterDataItemImageDTO masterDataItemImages) {
    ProductLevel3SummaryDetailsImage image = new ProductLevel3SummaryDetailsImage();
    image.setMainImage(masterDataItemImages.isMainImage());
    image.setSequence(masterDataItemImages.getSequence());
    image.setLocationPath(masterDataItemImages.getLocationPath());
    image.setMarkForDelete(false);
    image.setReviewType(StringUtils.EMPTY);
    return image;
  }

  @Override
  public CampaignPriceRequest convertItemSkusToCampaignPriceRequest(List<String> itemSkus, String categoryCode,
      Map<String, String> itemSkuAndPickupCodeMap, Map<String, Double> l5AndOfferPriceMap) {
    CampaignPriceRequest campaignPriceRequest = new CampaignPriceRequest();
    List<CampaignPriceSkuRequest> campaignPriceSkuRequestList = new ArrayList<>();
    for (String itemSku : itemSkus) {
      CampaignPriceSkuRequest campaignPriceSkuRequest = new CampaignPriceSkuRequest();
      campaignPriceSkuRequest.setItemSku(itemSku);
      campaignPriceSkuRequest.setCategoryCode(categoryCode);
      campaignPriceSkuRequest.setPickUpPointCode(itemSkuAndPickupCodeMap.get(itemSku));
      campaignPriceSkuRequest.setSellingPrice(l5AndOfferPriceMap.get(CommonUtils.toL5Id(itemSku,
          itemSkuAndPickupCodeMap.get(itemSku))));
      campaignPriceSkuRequestList.add(campaignPriceSkuRequest);
    }
    campaignPriceRequest.setCampaignPriceSkuRequestList(campaignPriceSkuRequestList);
    return campaignPriceRequest;
  }

  @Override
  public Map<String, CampaignPriceSkuResponse> convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(
      List<CampaignPriceResponse> campaignPriceResponseList) {
    Map<String, CampaignPriceSkuResponse> priceSkuResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(campaignPriceResponseList)) {
      priceSkuResponseMap = campaignPriceResponseList.stream().filter(
          campaignPriceResponse -> CollectionUtils.isNotEmpty(campaignPriceResponse.getItemInfoToPriceResponse()))
          .flatMap(campaignPriceResponse -> campaignPriceResponse.getItemInfoToPriceResponse().stream()).collect(
              Collectors
                  .toMap(CampaignPriceSkuResponse::getItemSku, campaignPriceSkuResponse -> campaignPriceSkuResponse,
                      (a, b) -> a));
    }
    return priceSkuResponseMap;
  }

  @Override
  public ProductLevel3SummaryDetailsImage convertMasterDataProductImagesToProductLevel3SummaryDetailsImage(
      ProductImage masterDataItemImages) {
    ProductLevel3SummaryDetailsImage image = new ProductLevel3SummaryDetailsImage();
    image.setMainImage(masterDataItemImages.isMainImages());
    image.setSequence(masterDataItemImages.getSequence());
    image.setLocationPath(masterDataItemImages.getLocationPath());
    image.setMarkForDelete(false);
    image.setReviewType(StringUtils.EMPTY);
    return image;
  }

}
