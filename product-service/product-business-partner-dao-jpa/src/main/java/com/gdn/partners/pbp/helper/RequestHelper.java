package com.gdn.partners.pbp.helper;


import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.InventoryUpsertModel;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PCBAddEditVideoRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductMasterDataUpdateRequest;
import com.gda.mta.product.dto.ProductScoreV2Response;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.SyncStockUpdateOrInsertVo;
import com.gda.mta.product.dto.UpdateOrInsertStockVo;
import com.gda.mta.product.dto.response.ItemPickupPointSummaryResponse;
import com.gda.mta.product.dto.response.ProductL3ImageResponse;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.response.ProductL3PriceResponse;
import com.gda.mta.product.dto.response.ProductL3ViewConfigResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.XProdAttributeMigrationEventModel;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.util.BeanUtils;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import static com.gdn.partners.pbp.commons.constants.Constants.ZERO;

@Slf4j
public class RequestHelper {
  private static final String ITEM_SKU_REGEX = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$";
  private static final Pattern ITEM_SKU_PATTERN = Pattern.compile(ITEM_SKU_REGEX);
  private static final String NEW = "new";


  public static ItemPickupPointListingRequest toItemPickupPointListingRequest(
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) {
    ItemPickupPointListingRequest itemPickupPointListingRequest = new ItemPickupPointListingRequest();
    BeanUtils.copyProperties(itemPickupPointListingL3Request, itemPickupPointListingRequest);
    return itemPickupPointListingRequest;
  }

  public static InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO(
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(itemPickupPointListingResponse.getItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(itemPickupPointListingResponse.getPickUpPointCode());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(itemPickupPointListingResponse.getMerchantCode());
    return inventoryDetailInfoRequestDTO;
  }

  public static InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO(
      ProductItemBusinessPartner productItemBusinessPartner, String businessPartnerCode) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(productItemBusinessPartner.getGdnProductItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(productItemBusinessPartner.getPickupPointId());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(businessPartnerCode);
    return inventoryDetailInfoRequestDTO;
  }

  public static InventoryDetailInfoRequestDTO toInventoryDetailInfoRequestDTO(
    ItemResponseV2 itemResponseV2) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
      new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(itemResponseV2.getItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(itemResponseV2.getPickUpPointCode());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(itemResponseV2.getMerchantCode());
    return inventoryDetailInfoRequestDTO;
  }

  public static CampaignPriceRequest toCampaignPriceRequestV2(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) {
    List<CampaignPriceSkuRequest> campaignPriceSkuRequestList = new ArrayList<>();
    for (ItemPickupPointListingResponse itemPickupPointListingResponse : itemPickupPointListingResponseList) {
      campaignPriceSkuRequestList.add(new CampaignPriceSkuRequest(itemPickupPointListingResponse.getItemSku(),
          itemPickupPointListingResponse.getCategoryCode(), itemPickupPointListingResponse.getPickUpPointCode(),
          itemPickupPointListingResponse.getOriginalSellingPrice()));
    }
    return new CampaignPriceRequest(campaignPriceSkuRequestList);
  }

  public static CampaignPriceRequest toCampaignPriceRequest(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) {
    List<CampaignPriceSkuRequest> campaignPriceSkuRequestList = new ArrayList<>();
    for (ItemPickupPointListingResponse itemPickupPointListingResponse : itemPickupPointListingResponseList) {
      CampaignPriceSkuRequest campaignPriceSkuRequest = new CampaignPriceSkuRequest();
      campaignPriceSkuRequest.setItemSku(itemPickupPointListingResponse.getItemSku());
      campaignPriceSkuRequest.setCategoryCode(itemPickupPointListingResponse.getCategoryCode());
      campaignPriceSkuRequestList.add(campaignPriceSkuRequest);
    }
    return new CampaignPriceRequest(campaignPriceSkuRequestList);
  }

  public static ItemPickupPointSummaryRequest toItemPickupPointSummaryRequest(
      String businessPartnerCode, List<String> productSkuList, boolean onlineOrCnc,
      List<String> accessiblePickupPoints) {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest =
        ItemPickupPointSummaryRequest.builder().merchantCode(businessPartnerCode)
            .productSkuList(productSkuList).build();
    if (onlineOrCnc) {
      itemPickupPointSummaryRequest.setOnlineOrCnc(true);
    }
    if (CollectionUtils.isNotEmpty(accessiblePickupPoints)) {
      itemPickupPointSummaryRequest.setPickupPointCodes(accessiblePickupPoints);
    }
    return itemPickupPointSummaryRequest;
  }

  public static ProductSummaryRequest toProductSummaryRequest(ProductL3ListingRequest request) {
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    BeanUtils.copyProperties(request, productSummaryRequest);
    return productSummaryRequest;
  }

  public static void toL5InventoryResponse(InventoryDetailInfoResponseV2DTO inventoryL5,
      ItemPickupPointSummaryResponse itemPickupPointSummaryWebResponse) {
    WebInventoryResponseDTO webInventory = inventoryL5.getWebInventoryResponse();
    itemPickupPointSummaryWebResponse.setAvailableStockLevel2(webInventory.getAvailableStock());
    itemPickupPointSummaryWebResponse.setReservedStockLevel2(
        webInventory.getOriginalStock() - webInventory.getAvailableStock());
    itemPickupPointSummaryWebResponse.setSynchronizeStock(webInventory.isSyncStock());
    List<WarehouseInventoryResponseDTO> warehouseInventory =
        inventoryL5.getWarehouseInventoryResponseList();
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(warehouseInventory)) {
      int warehouseAvailable = 0;
      int warehouseOrigin = 0;
      for (WarehouseInventoryResponseDTO warehouseInv : warehouseInventory) {
        warehouseAvailable += warehouseInv.getAvailableStock();
        warehouseOrigin += warehouseInv.getOriginalStock();
      }
      itemPickupPointSummaryWebResponse.setAvailableStockLevel1(warehouseAvailable);
      itemPickupPointSummaryWebResponse.setReservedStockLevel1(
          warehouseOrigin - warehouseAvailable);
    }
  }

  public static void toL5CampaignResponse(CampaignPriceSkuResponse campaignPriceSkuResponse,
      ItemPickupPointSummaryResponse itemPickupPointSummaryWebResponse,
      boolean priceEditDisabled) {
    if (campaignPriceSkuResponse.isLockPriceUpdate() || campaignPriceSkuResponse.isLive()) {
      itemPickupPointSummaryWebResponse.setPriceEditDisabled(true);
    } else {
      itemPickupPointSummaryWebResponse.setPriceEditDisabled(priceEditDisabled);
    }
    itemPickupPointSummaryWebResponse.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
    itemPickupPointSummaryWebResponse.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
    itemPickupPointSummaryWebResponse.setMinAllowedPrice(campaignPriceSkuResponse.getMinAllowedPrice());
    itemPickupPointSummaryWebResponse.setMaxAllowedPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
    if (Objects.nonNull(campaignPriceSkuResponse.getCampaignPrice())) {
      itemPickupPointSummaryWebResponse.setCampaignPrice(campaignPriceSkuResponse.getCampaignPrice());
    }
  }

  public static List<ProductL3PriceResponse> toItemPrice(Collection<PriceDTO> priceDTOS) {
    List<ProductL3PriceResponse> priceWebResponses = new ArrayList<>();
    for (PriceDTO priceDTO : priceDTOS) {
      ProductL3PriceResponse priceResponse = new ProductL3PriceResponse();
      priceResponse.setChannelId(priceDTO.getChannel());
      priceResponse.setPrice(priceDTO.getListPrice());
      priceResponse.setSalePrice(priceDTO.getOfferPrice());
      if (org.apache.commons.collections.CollectionUtils.isNotEmpty(priceDTO.getListOfDiscountPrices())) {
        priceResponse.setDiscountAmount(priceDTO.getListOfDiscountPrices().get(0).getDiscountPrice());
        priceResponse.setDiscountStartDate(priceDTO.getListOfDiscountPrices().get(0).getStartDateTime());
        priceResponse.setDiscountEndDate(priceDTO.getListOfDiscountPrices().get(0).getEndDateTime());
        priceResponse.setPromotionName(priceDTO.getListOfDiscountPrices().get(0).getAdjustmentName());
      }
      priceWebResponses.add(priceResponse);
    }
    return priceWebResponses;
  }

  public static List<ProductL3ImageResponse> toItemImageList(List<MasterDataItemImageDTO> imageDTOList) {
    List<ProductL3ImageResponse> imageList = new ArrayList<>();
    if (org.apache.commons.collections.CollectionUtils.isNotEmpty(imageDTOList)) {
      for (MasterDataItemImageDTO image : imageDTOList) {
        ProductL3ImageResponse imageWebResponse = new ProductL3ImageResponse();
        imageWebResponse.setMainImage(image.isMainImage());
        imageWebResponse.setLocationPath(image.getLocationPath());
        imageWebResponse.setSequence(image.getSequence());
        imageList.add(imageWebResponse);
      }
    }
    return imageList;
  }

  public static List<ProductL3ViewConfigResponse> toItemViewConfigList(
      List<ItemViewConfigDTO> viewConfigDTOList) {
    List<ProductL3ViewConfigResponse> configList = new ArrayList<>();
    for (ItemViewConfigDTO viewConfig : viewConfigDTOList) {
      ProductL3ViewConfigResponse viewConfigWebResponse = new ProductL3ViewConfigResponse();
      viewConfigWebResponse.setChannelId(viewConfig.getChannel());
      viewConfigWebResponse.setBuyable(viewConfig.isBuyable());
      viewConfigWebResponse.setDisplay(viewConfig.isDiscoverable());
      configList.add(viewConfigWebResponse);
    }
    return configList;
  }

  public static ProductL3ListingResponse toProductL3ListingResponse(
      ProductL3SummaryResponse productL3SummaryResponse, Map<String, String> skuXCategoryName,
      Map<String, String> skuXSuspensionReasonMap,
      Map<String, InventoryStockInfoDTO> productSkuInventoryMap,
      Map<String, InventoryDetailInfoResponseV2DTO> itemInventoryMapL5,
      Map<String, CampaignPriceSkuResponse> itemCampaignMap, String productDetailLink) {
    String productSku = productL3SummaryResponse.getProductSku();
    com.gdn.x.product.rest.web.model.response.ProductScoreResponse scoreResponse =
        productL3SummaryResponse.getProductScore();
    ProductL3ListingResponse productL3ListingResponse = new ProductL3ListingResponse();
    BeanUtils.copyProperties(productL3SummaryResponse, productL3ListingResponse,
        "ItemL4SummaryResponse", "ProductScoreResponse");
    productL3ListingResponse.setOff2OnActiveFlag(productL3SummaryResponse.isOff2OnChannelActive());
    productL3ListingResponse.setFreeSample(productL3SummaryResponse.isFreeSample());
    productL3ListingResponse.setCategoryName(skuXCategoryName.get(productSku));
    productL3ListingResponse.setFbbActivated(productL3SummaryResponse.isFbbActivated());
    productL3ListingResponse.setSyncronizeStock(productL3SummaryResponse.isSynchronized());
    if (MapUtils.isNotEmpty(skuXSuspensionReasonMap)) {
      productL3ListingResponse.setSuspensionReason(skuXSuspensionReasonMap.get(productSku));
    } else {
      InventoryStockInfoDTO inventoryL3 = productSkuInventoryMap.get(productSku);
      if (Objects.nonNull(inventoryL3)) {
        productL3ListingResponse.setAvailableStockLevel1(inventoryL3.getWarehouseTotalAvailableStock());
        productL3ListingResponse.setAvailableStockLevel2(inventoryL3.getWebTotalAvailableStock());
        productL3ListingResponse.setReservedStockLevel1(inventoryL3.getWarehouseTotalOriginalStock()
            - inventoryL3.getWarehouseTotalAvailableStock());
        productL3ListingResponse.setReservedStockLevel2(
            inventoryL3.getWebTotalOriginalStock() - inventoryL3.getWebTotalAvailableStock());
        productL3ListingResponse.setTotalWebStock(inventoryL3.getWebTotalAvailableStock());
        productL3ListingResponse.setTotalWarehouseStock(inventoryL3.getWarehouseTotalAvailableStock());
        productL3ListingResponse.setTotalActiveStock(inventoryL3.getTotalStock());
      } else {
        productL3ListingResponse.setShowL3Stock(false);
      }
      productL3ListingResponse.setProductDetailPageLink(productDetailLink);
      ProductScoreV2Response productScoreResponse = null;
      if (Objects.nonNull(scoreResponse)) {
        productScoreResponse = ProductScoreV2Response.builder()
            .MANDATORY_INFO_RULE(scoreResponse.getMandatoryAttributeScore())
            .PRODUCT_TITLE_RULE(scoreResponse.getProductTitleScore())
            .IMAGE_RULE(scoreResponse.getImageScore())
            .DESCRIPTION_RULE(scoreResponse.getDescriptionScore())
            .EAN_UPC_RULE(scoreResponse.getEanUpcScore())
            .RECOMMENDED_ATTRIBUTE_RULE(scoreResponse.getRecommendedAttributeScore())
            .REMAINING_ATTRIBUTE_RULE(scoreResponse.getRemainingAttributeScore())
            .USP_RULE(scoreResponse.getUspScore())
            .VARIANT_CREATING_RULE(scoreResponse.getVariantCreatingScore())
            .TOTAL_SCORE(scoreResponse.getTotalScore())
            .VIDEO_URL_RULE(scoreResponse.getVideoUrlScore()).build();
      }
      productL3ListingResponse.setProductScore(productScoreResponse);
      if ((productL3SummaryResponse.getVariantCount() == Constants.NO_VARIANTS_COUNT)
          && Objects.nonNull(productL3SummaryResponse.getItemL4SummaryResponse())) {
        ItemPickupPointSummaryResponse itemPickupPointSummaryResponse = new ItemPickupPointSummaryResponse();
        String l5Id = toL5Id(productL3SummaryResponse.getItemL4SummaryResponse().getItemSku(),
            productL3SummaryResponse.getItemL4SummaryResponse().getPickupPointCode());
        InventoryDetailInfoResponseV2DTO inventoryL5 = itemInventoryMapL5.get(l5Id);
        CampaignPriceSkuResponse campaignL4 = itemCampaignMap.get(l5Id);
        BeanUtils.copyProperties(productL3SummaryResponse.getItemL4SummaryResponse(),
            itemPickupPointSummaryResponse);
        if (org.apache.commons.collections.CollectionUtils.isNotEmpty(
            itemPickupPointSummaryResponse.getActivePromoBundlings())) {
          itemPickupPointSummaryResponse.getActivePromoBundlings()
              .remove(Constants.WHOLESALE_PRICE);
        }
        if (Objects.nonNull(inventoryL5)) {
          toL5InventoryResponse(inventoryL5, itemPickupPointSummaryResponse);
          productL3ListingResponse.setSyncronizeStock(inventoryL5.getWebInventoryResponse().isSyncStock());
          productL3ListingResponse.setTotalWebStock(inventoryL5.getWebInventoryResponse().getAvailableStock());
          productL3ListingResponse.setTotalWarehouseStock(
              Optional.ofNullable(inventoryL5.getWarehouseInventoryResponseList()).orElse(new ArrayList<>()).stream()
                  .map(WarehouseInventoryResponseDTO::getAvailableStock)
                  .collect(Collectors.summingInt(Integer::intValue)));
          productL3ListingResponse.setTotalActiveStock(inventoryL5.getWebInventoryResponse().isSyncStock() ?
              productL3ListingResponse.getTotalWarehouseStock() :
              productL3ListingResponse.getTotalWebStock());
          productL3ListingResponse.setShowL3Stock(true);
        }
        if (Objects.nonNull(campaignL4)) {
          toL5CampaignResponse(campaignL4, itemPickupPointSummaryResponse,
              productL3SummaryResponse.getItemL4SummaryResponse().isPriceEditDisabled());
        }
        itemPickupPointSummaryResponse.setPrices(
            toItemPrice(productL3SummaryResponse.getItemL4SummaryResponse().getPrice()));
        itemPickupPointSummaryResponse.setItemName(
            productL3SummaryResponse.getItemL4SummaryResponse().getGeneratedItemName());
        itemPickupPointSummaryResponse.setLateFulfillment(
            productL3SummaryResponse.getItemL4SummaryResponse().getIsLateFulfillment());
        itemPickupPointSummaryResponse.setImages(toItemImageList(
            productL3SummaryResponse.getItemL4SummaryResponse().getMasterDataItemImages()));
        itemPickupPointSummaryResponse.setViewConfigs(toItemViewConfigList(
            productL3SummaryResponse.getItemL4SummaryResponse().getItemViewConfigs()));
        productL3ListingResponse.setItemPickupPointSummary(itemPickupPointSummaryResponse);
      }
    }
    return productL3ListingResponse;
  }

  public static String toProductDetailPage(String productSku, String productDetailPageUrlPrefix) {
    String productDetailLink =
        Optional.ofNullable(productSku).map(sku -> sku.replace(Constants.HYPHEN, Constants.DOT))
            .map(sku -> String.join(Constants.DOT, sku, "html"))
            .orElse(org.apache.commons.lang3.StringUtils.EMPTY);
    return String.join("-", productDetailPageUrlPrefix, productDetailLink);
  }

  public static String toL5Id(String webItemSku, String pickupPointCode) {
    return new StringBuilder().append(webItemSku).append(Constants.HYPHEN)
        .append(pickupPointCode).toString();
  }

  public static boolean validateSalePrice(double salePrice, double normalPrice) {
    return salePrice > normalPrice;
  }

  private static void performCncValidation(QuickEditV2Request quickEditV2Request) {
    if (ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getStatus()) || ProductLevel3Status.OFFLINE.equals(
        quickEditV2Request.getCncStatus())) {
      return;
    }
    if (!Objects.equals(quickEditV2Request.getStatus(), quickEditV2Request.getCncStatus())) {
      quickEditV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
    }
  }


  public static QuickEditV2Request populateQuickEditV2Request(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse itemSummaryListResponse, String status, String cncStatus,
      boolean cncForWarehouseFeatureSwitch) {
    quickEditV2Request.setFbbActivated(itemSummaryListResponse.isFbbActivated());
    if (Objects.isNull(quickEditV2Request.getSellerSku())) {
      quickEditV2Request.setSellerSku(
        Optional.ofNullable(itemSummaryListResponse).map(ItemSummaryListResponse::getMerchantSku)
          .orElse(null));
    }
    if (Objects.isNull(quickEditV2Request.getOff2OnActiveFlag())) {
      quickEditV2Request.setOff2OnActiveFlag(Optional.ofNullable(itemSummaryListResponse)
        .map(ItemSummaryListResponse::isOff2OnChannelActive).orElse(null));
    }
    if (Objects.isNull(quickEditV2Request.getWholeSaleActivated())) {
      quickEditV2Request.setWholeSaleActivated(
        Optional.of(itemSummaryListResponse).map(ItemSummaryListResponse::getWholesalePriceActivated).orElse(null));
    }
    if (Objects.isNull(quickEditV2Request.getStatus())) {
      quickEditV2Request.setStatus(ProductLevel3Status.valueOf(status));
    }
    if (cncForWarehouseFeatureSwitch && Objects.isNull(quickEditV2Request.getCncStatus())) {
      quickEditV2Request.setCncStatus(ProductLevel3Status.valueOf(cncStatus));
    }
    if (cncForWarehouseFeatureSwitch) {
      performCncValidation(quickEditV2Request);
    }

    if (validateEmptyPriceRequest(quickEditV2Request) || validateSalePrice(
        quickEditV2Request.getPrice().getSalePrice(), quickEditV2Request.getPrice().getPrice())) {
      Set<PriceDTO> price = itemSummaryListResponse.getPrice();
      for (PriceDTO productLevel3Price : price) {
        if (Objects.nonNull(quickEditV2Request.getPrice())) {
          if (Objects.isNull(quickEditV2Request.getPrice().getSalePrice())) {
            quickEditV2Request.getPrice()
              .setSalePrice(Optional.of(productLevel3Price.getOfferPrice()).get());
          }
          if (Objects.isNull(quickEditV2Request.getPrice().getPrice())) {
            quickEditV2Request.getPrice()
              .setPrice(Optional.of(productLevel3Price.getListPrice()).get());
          }
          if (validateSalePrice(quickEditV2Request.getPrice().getSalePrice(),
              quickEditV2Request.getPrice().getPrice())) {
            quickEditV2Request.getPrice().setPrice(Optional.of(productLevel3Price.getListPrice()).get());
            quickEditV2Request.getPrice().setSalePrice(Optional.of(productLevel3Price.getOfferPrice()).get());
          }
          quickEditV2Request.getPrice()
            .setChannelId(Optional.of(productLevel3Price.getChannel()).get());
          if (!Optional.of(quickEditV2Request).map(QuickEditV2Request::getPrice)
              .map(ProductLevel3PriceRequest::getDiscountAmount).isPresent()) {
            if (Objects.nonNull(productLevel3Price.getMerchantPromoDiscountPrice())) {
              quickEditV2Request.getPrice().setDiscountAmount(
                  Optional.of(productLevel3Price.getMerchantPromoDiscountPrice())
                      .map(DiscountPriceDTO::getDiscountPrice).get());
              quickEditV2Request.getPrice().setDiscountEndDate(
                  Optional.of(productLevel3Price.getMerchantPromoDiscountPrice()).map(DiscountPriceDTO::getEndDateTime)
                      .orElse(new Date()));
              quickEditV2Request.getPrice().setDiscountStartDate(
                  Optional.of(productLevel3Price.getMerchantPromoDiscountPrice())
                      .map(DiscountPriceDTO::getStartDateTime).orElse(new Date()));
            }
          }
        } else {
            quickEditV2Request.setPrice(
                new ProductLevel3PriceRequest(productLevel3Price.getChannel(), productLevel3Price.getListPrice(),
                    productLevel3Price.getOfferPrice(),
                    Optional.ofNullable(productLevel3Price).map(PriceDTO::getMerchantPromoDiscountPrice)
                        .map(DiscountPriceDTO::getDiscountPrice).orElse(null),
                    Optional.ofNullable(productLevel3Price).map(PriceDTO::getMerchantPromoDiscountPrice)
                        .map(DiscountPriceDTO::getStartDateTime).orElse(null),
                    Optional.ofNullable(productLevel3Price).map(PriceDTO::getMerchantPromoDiscountPrice)
                        .map(DiscountPriceDTO::getEndDateTime).orElse(null), null));
        }
      }

    }
    return quickEditV2Request;
  }

  private static Boolean validateEmptyPriceRequest(QuickEditV2Request quickEditV2Request) {
    if (Objects.isNull(quickEditV2Request.getPrice()))
      return true;
    else if (Stream.of(quickEditV2Request.getPrice().getPrice(),
        quickEditV2Request.getPrice().getSalePrice(), Objects.isNull(quickEditV2Request.getPrice().getDiscountAmount()))
      .anyMatch(Objects::isNull)) {
      return true;
    }
    return false;
  }

  public static WholesalePriceSkuDetailListRequest toWholesalePriceSkuDetailListRequest(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) {
    Set<String> itemSkus = itemPickupPointListingResponseList.stream().map(ItemPickupPointListingResponse::getItemSku)
        .collect(Collectors.toSet());
    List<ItemInfoDto> itemInfoDtoList = itemPickupPointListingResponseList.stream().map(
        itemPickupPointListingResponse -> ItemInfoDto.builder()
          .itemSku(itemPickupPointListingResponse.getItemSku())
          .pickupPointCode(itemPickupPointListingResponse.getPickUpPointCode()).itemPickupPointId(
            CommonUtils.getItemSkuAndPickupPointKey(itemPickupPointListingResponse.getItemSku(),
              itemPickupPointListingResponse.getPickUpPointCode())).build())
      .collect(Collectors.toList());
    return new WholesalePriceSkuDetailListRequest(itemSkus, itemInfoDtoList);
  }

  public static List<InventoryDetailInfoRequestDTO> toInventoryDetailInfoRequestList(
    List<ProductL5DetailResponse> productL5DetailsList) {
    return productL5DetailsList.stream().map(RequestHelper::toInventoryRequestDto)
      .collect(Collectors.toList());
  }

  public static InventoryDetailInfoRequestDTO toInventoryRequestDto(
    ProductL5DetailResponse productL5Detail) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
      new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(productL5Detail.getItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(productL5Detail.getPickupPointCode());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(productL5Detail.getMerchantCode());
    return inventoryDetailInfoRequestDTO;
  }

  public static CampaignPriceRequest toCampaignPriceRequestFromProductDetail(
    List<ProductL5DetailResponse> productL5DetailResponses) {
    List<CampaignPriceSkuRequest> campaignPriceSkuRequestList = new ArrayList<>();
    for (ProductL5DetailResponse productL5DetailResponse : productL5DetailResponses) {
      campaignPriceSkuRequestList.add(
        new CampaignPriceSkuRequest(productL5DetailResponse.getItemSku(),
          productL5DetailResponse.getCategoryCode(), productL5DetailResponse.getPickupPointCode(),
          productL5DetailResponse.getPrices().stream().findFirst().orElse(new PriceResponse())
            .getSalePrice()));
    }
    return new CampaignPriceRequest(campaignPriceSkuRequestList);
  }

  public static List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> toItemPickupPointRequest(
    List<ItemSkuPpCodeRequest> itemSkuPpCodeRequests) {
    return itemSkuPpCodeRequests.stream().map(RequestHelper::toItemPickupPointRequest)
      .collect(Collectors.toList());
  }

  public static com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest toItemPickupPointRequest(
    ItemSkuPpCodeRequest itemSkuPpCodeRequest) {
    return new com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest(
      itemSkuPpCodeRequest.getItemSku(), itemSkuPpCodeRequest.getPickupPointCode());
  }

  public static void validateAddPickupPointRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap, boolean enableAddPickupPointCheck) {
    if (enableAddPickupPointCheck) {
      productVariantUpdateRequest.setAddPickupPoints(productVariantUpdateRequest.getAddPickupPoints().stream()
          .filter(itemPickupPointRequest -> !savedItemPickupPointDataMap.keySet().contains(
              CommonUtils.getItemSkuAndPickupPointKey(itemPickupPointRequest.getItemSku(),
                  itemPickupPointRequest.getPickupPointId()))).collect(Collectors.toList()));
    }
  }

  public static boolean isEligibleForXProductUpdate(List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests,
      List<ItemPickupPointQuickEditRequest> addPickupPointRequests,
      List<ItemPickupPointDeleteRequest> deletePickupPointRequests, boolean isOnlineFlagChanged,
      boolean isCNCFlagChangedAtL3Level, boolean isFbbFlagChangedAtL3Level,
      AddDeleteVariantRequest addDeleteVariantRequest, EditFlagChangesDTO editFlagChangesDTO,
      List<ProductBundleRecipeRequest> productBundleRecipe) {
    return CollectionUtils.isNotEmpty(quickEditUpdateRequests) || org.apache.commons.collections.CollectionUtils.isNotEmpty(addPickupPointRequests)
        || CollectionUtils.isNotEmpty(deletePickupPointRequests) || Boolean.TRUE.equals(isOnlineFlagChanged)
        || Boolean.TRUE.equals(isCNCFlagChangedAtL3Level) || Boolean.TRUE.equals(isFbbFlagChangedAtL3Level)
        || Objects.nonNull(addDeleteVariantRequest) || editFlagChangesDTO.isB2bFlagChangedAtL3Level()
        || editFlagChangesDTO.isB2cFlagChangedAtL3Level() || CollectionUtils.isNotEmpty(productBundleRecipe);
  }

  public static boolean isItemSku(String sku) {
    if (StringUtils.isNotBlank(sku)) {
      Matcher matcher = ITEM_SKU_PATTERN.matcher(sku);
      return matcher.matches();
    }
    return false;
  }

  public static void removeProductsFromBundleRecipeWhichDontHaveItemSku(ProductCreationRequest productCreationRequest) {
    Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
        .filter(productItemCreationRequest -> CollectionUtils.isNotEmpty(productItemCreationRequest.getBundleRecipe()))
        .forEach(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().removeIf(
            bundleRecipeRequest -> Objects.isNull(bundleRecipeRequest) || StringUtils.isBlank(
                bundleRecipeRequest.getItemSku())));

  }

  public static List<String> getProductSkuForBundleProductChildSkus(ProductCreationRequest productCreationRequest) {
    return Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
        .flatMap(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().stream()).map(
            bundleRecipeRequest -> bundleRecipeRequest.getItemSku()
                .substring(0, bundleRecipeRequest.getItemSku().lastIndexOf(Constants.HYPHEN))).distinct()
        .collect(Collectors.toList());
  }

  public static List<String> getItemSkuForBundleProductChildSkus(ProductCreationRequest productCreationRequest) {
    return Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
        .flatMap(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().stream())
        .map(com.gda.mta.product.dto.BundleRecipeRequest::getItemSku).distinct().collect(Collectors.toList());
  }

  public static void setBundleProductRequests(ProductVariantUpdateRequest productVariantUpdateRequest,
      ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductBundleRecipe())) {
      Set<BundleRecipeRequest> bundleRecipeRequestSet = new HashSet<>();
      for (ProductBundleRecipeRequest productBundleRecipeRequest : productVariantUpdateRequest.getProductBundleRecipe()) {
        BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
        bundleRecipeRequest.setItemSku(productBundleRecipeRequest.getItemSku());
        Set<BundleRecipeVo> bundleRecipeVoList = new HashSet<>();
        for (ProductBundleRecipe productBundleRecipe : productBundleRecipeRequest.getBundleRecipe()) {
          BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
          bundleRecipeVo.setItemSku(productBundleRecipe.getItemSku());
          bundleRecipeVo.setQuantity(productBundleRecipe.getQuantity());
          bundleRecipeVoList.add(bundleRecipeVo);
        }
        bundleRecipeRequest.setBundleRecipe(bundleRecipeVoList);
        bundleRecipeRequestSet.add(bundleRecipeRequest);
      }
      itemPickupPointUpdateRequest.setBundleRecipesRequests(bundleRecipeRequestSet);
    }
  }

  public static boolean isL5Updated(EditItemResponse editItemResponse,
      CombinedEditItemResponse combinedEditItemResponse) {
    return Objects.nonNull(editItemResponse) || Objects.nonNull(combinedEditItemResponse);
  }

  public static List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> getBillOfMaterialSetup(
      Set<ProductBundleRecipe> bundleRecipeRequests, Map<String, String> itemSkuAndItemCodeMap) {
    List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> requestList =
        new ArrayList<>();
    for (ProductBundleRecipe productBundleRecipe : bundleRecipeRequests) {
      CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest materialSetupRequest =
          new CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest();
      materialSetupRequest.setItemCode(itemSkuAndItemCodeMap.get(productBundleRecipe.getItemSku()));
      materialSetupRequest.setQuantity(productBundleRecipe.getQuantity());
      requestList.add(materialSetupRequest);
    }
    return requestList;
  }

  public static List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> getBillOfMaterialSetupOnActivation(
      Set<BundleRecipeVo> bundleRecipeRequests, Map<String, String> itemSkuAndItemCodeMap) {
    List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> requestList =
        new ArrayList<>();
    if(Objects.nonNull(bundleRecipeRequests)){
      for (BundleRecipeVo productBundleRecipe : bundleRecipeRequests) {
        CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest
            materialSetupRequest =
            new CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest();
        materialSetupRequest.setItemCode(
            itemSkuAndItemCodeMap.get(productBundleRecipe.getItemSku()));
        materialSetupRequest.setQuantity(productBundleRecipe.getQuantity());
        requestList.add(materialSetupRequest);
      }
    }
    return requestList;
  }

  public static ProductLevel3Inventory getInventoryInsertRequest(ProfileResponse profileResponse,
      String businessPartnerCode, String itemCode, String productSku, String itemSku,
      String pickupPointCode, Integer stock, Integer minimumStock, Boolean syncStock,
      Boolean fbbActive, boolean mppForWhEnabled, boolean faasFeatureSwitch,
      Integer initialPreOrderQuota, Date preOrderDate, boolean preOrderFeatureSwitch, boolean distributionPickupPoint)
      throws Exception {
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setWebItemSku(itemSku);
    productLevel3Inventory.setWebPickupPointCode(pickupPointCode);
    productLevel3Inventory.setWebSyncStock(
        Boolean.TRUE.equals(syncStock) && !isFaasEligibleSeller(faasFeatureSwitch, profileResponse));
    productLevel3Inventory.setWebAvailable(stock);
    productLevel3Inventory.setWebMinAlert(minimumStock);
    productLevel3Inventory.setWebMerchantCode(businessPartnerCode);
    productLevel3Inventory.setWarehouseItemSku(itemCode);
    productLevel3Inventory.setProductSku(productSku);
    productLevel3Inventory.setFbbPP(Boolean.TRUE.equals(fbbActive) && mppForWhEnabled);
    productLevel3Inventory.setDistributionPickupPoint(distributionPickupPoint);
    if (preOrderFeatureSwitch && shouldPopulatePreOrderDetails(profileResponse,
        preOrderDate)) {
      productLevel3Inventory.setPreOrderDate(preOrderDate);
      productLevel3Inventory.setInitialPreOrderQuota(
          ObjectUtils.defaultIfNull(initialPreOrderQuota, ZERO));
    }
    if (isPurchaseOrderPurchaseTerm(profileResponse)) {
      productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
    } else {
      productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
    }
    return productLevel3Inventory;
  }

  public static boolean isPreOrderDateValid(Date preOrderDate) {
    return Objects.nonNull(preOrderDate) && preOrderDate.after(new Date());
  }

  public static boolean shouldPopulatePreOrderDetails(ProfileResponse businessPartner,
      Date preOrderDate) {
    if (!getBusinessPartnerFlagValue(businessPartner, Constants.BLIBLI_OMG)) {
      return false;
    }
    return isPreOrderDateValid(preOrderDate);
  }

  private static boolean isPurchaseOrderPurchaseTerm(ProfileResponse businessPartner) throws Exception {
    return GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equals(businessPartner.getCompany().getPurchaseTerm());
  }

  public static boolean hasInvalidImagePath(String productCode,
      List<ProductLevel3SummaryDetailsImageRequest> variantRequest, boolean productCodeLocationValidate,
      boolean validateImagePathEnabled) {
    if (validateImagePathEnabled) {
      return Optional.ofNullable(variantRequest).orElse(new ArrayList<>()).stream().filter(Objects::nonNull).anyMatch(
          imageRequest -> productCodeLocationValidate ?
              StringUtils.equalsIgnoreCase(NEW, imageRequest.getReviewType()) && (
                  StringUtils.isBlank(imageRequest.getLocationPath()) || !imageRequest.getLocationPath()
                      .contains(productCode)) :
              StringUtils.equalsIgnoreCase(NEW, imageRequest.getReviewType()) && (StringUtils.isBlank(
                  imageRequest.getLocationPath())));
    } else {
      return false;
    }
  }

  public static List<InventoryUpsertModel> toInventoryUpsertModels(PickupPointUpdateRequest pickupPointUpdateRequest,
      Map<String, Pair<String, String>> itemXOldPickUpPoint) {
    List<InventoryUpsertModel> inventoryUpsertModels = new ArrayList<>();
    for (PickupPointRequest pickupPointRequest : pickupPointUpdateRequest.getItemsPickupPoint()) {
      InventoryUpsertModel inventoryUpsertModel =
          InventoryUpsertModel.builder().businessPartnerCode(pickupPointUpdateRequest.getBusinessPartnerCode())
              .itemCode(
                  Optional.ofNullable(itemXOldPickUpPoint.get(pickupPointRequest.getItemSku())).map(Pair::getRight)
                      .orElse(null)).productSku(pickupPointUpdateRequest.getProductSku())
              .itemSku(pickupPointRequest.getItemSku()).oldPickupPointCode(
                  Optional.ofNullable(itemXOldPickUpPoint.get(pickupPointRequest.getItemSku())).map(Pair::getLeft)
                      .orElse(null)).newPickupPointCode(pickupPointRequest.getPickupPointCode()).stock(Constants.ZERO)
              .minimumStock(Constants.ZERO).syncStock(pickupPointUpdateRequest.getFbbActivated())
              .fbbActive(pickupPointUpdateRequest.getFbbActivated()).build();
      inventoryUpsertModels.add(inventoryUpsertModel);
    }
    return inventoryUpsertModels;
  }

  public static List<InventoryUpsertModel> toInventoryUpsertModels(String businessPartnerCode, String productSku,
      List<WebInventoryUpdatePickupPointRequestDTO> inventoryRequest, Boolean fbbActivated,
      Map<String, Pair<String, String>> itemXOldPickUpPoint) {
    List<InventoryUpsertModel> inventoryUpsertModels = new ArrayList<>();
    for (WebInventoryUpdatePickupPointRequestDTO webInventoryUpdatePickupPointRequestDTO : inventoryRequest) {
      InventoryUpsertModel inventoryUpsertModel =
          InventoryUpsertModel.builder().businessPartnerCode(businessPartnerCode).itemCode(
                  Optional.ofNullable(itemXOldPickUpPoint.get(webInventoryUpdatePickupPointRequestDTO.getWebItemSku()))
                      .map(Pair::getRight).orElse(null)).productSku(productSku)
              .itemSku(webInventoryUpdatePickupPointRequestDTO.getWebItemSku()).oldPickupPointCode(
                  Optional.ofNullable(itemXOldPickUpPoint.get(webInventoryUpdatePickupPointRequestDTO.getWebItemSku()))
                      .map(Pair::getLeft).orElse(null))
              .newPickupPointCode(webInventoryUpdatePickupPointRequestDTO.getNewPickupPointCode()).stock(Constants.ZERO)
              .minimumStock(Constants.ZERO).syncStock(fbbActivated).fbbActive(fbbActivated).build();
      inventoryUpsertModels.add(inventoryUpsertModel);
    }
    return inventoryUpsertModels;
  }

  public static List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> toWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(
      List<InventoryUpsertModel> inventoryUpsertModels) {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS =
        new ArrayList<>();
    for (InventoryUpsertModel inventoryUpsertModel : inventoryUpsertModels) {
      WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO =
          new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(inventoryUpsertModel.getItemSku(),
              inventoryUpsertModel.getBusinessPartnerCode(), inventoryUpsertModel.getOldPickupPointCode());
      webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS.add(webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO);
    }
    return webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS;
  }

  public static List<ProductLevel3Inventory> toProductLevel3InventoryList(ProfileResponse profileResponse,
      List<InventoryUpsertModel> inventoryUpsertModels, boolean mppForWhEnabled, boolean faasFeatureSwitch)
      throws Exception {
    List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
    for (InventoryUpsertModel inventoryUpsertModel : inventoryUpsertModels) {
      productLevel3InventoryList.add(
          getInventoryInsertRequest(profileResponse, inventoryUpsertModel.getBusinessPartnerCode(),
              inventoryUpsertModel.getItemCode(), inventoryUpsertModel.getProductSku(),
              inventoryUpsertModel.getItemSku(), inventoryUpsertModel.getNewPickupPointCode(),
              inventoryUpsertModel.getStock(), inventoryUpsertModel.getMinimumStock(),
              inventoryUpsertModel.getSyncStock(), inventoryUpsertModel.getFbbActive(), mppForWhEnabled,
              faasFeatureSwitch, 0, null, false, false));
    }
    return productLevel3InventoryList;
  }

  public static boolean isEligibleForPCBUpdate(ProductDetailEditDTO productDetailEditDTO,
      EditProductDetailRequest editProductDetailRequest) {
    return
        Objects.isNull(editProductDetailRequest.getProductRequest()) && (productDetailEditDTO.isEligibleForPCBUpdate()
            || CollectionUtils.isNotEmpty(productDetailEditDTO.getProductImageEditRequests()))
            || CollectionUtils.isNotEmpty(productDetailEditDTO.getProductItemUpcCodeUpdateRequests());
  }

  public static List<ItemActivationRequest> getItemsForAddDeleteRetryList(Map<String, Boolean> itemSkuMarkForDeleteMap,
      Map<String, String> itemSkuItemCodeMap, List<ItemActivationRequest> itemActivationRequestList) {
    List<ItemActivationRequest> ItemsForAddDeleteRetryList = new ArrayList<>();
    for (ItemActivationRequest itemActivationRequest : itemActivationRequestList) {
      ItemActivationRequest activationRequest = new ItemActivationRequest();
      activationRequest.setMarkForDelete(itemSkuMarkForDeleteMap.get(itemActivationRequest.getItemSku()));
      activationRequest.setItemCode(itemSkuItemCodeMap.get(itemActivationRequest.getItemSku()));
      BeanUtils.copyProperties(itemActivationRequest, activationRequest, "markForDelete", "itemCode");
      ItemsForAddDeleteRetryList.add(activationRequest);
    }
    return ItemsForAddDeleteRetryList;
  }

  public static UpdateOrInsertStockVo getUpdateOrInsertStockVo(boolean mppForWhEnabled,
      String businessPartnerCode, ProfileResponse profileResponse,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ItemPickupPointRequest itemPickupPointRequest, boolean updatePreOrderQuota,
      PreOrderRequest preOrderRequest) {
    return UpdateOrInsertStockVo.builder().businessPartnerCode(businessPartnerCode)
        .productSku(productVariantPriceStockAndImagesRequest.getProductSku())
        .itemSku(itemPickupPointRequest.getItemSku()).itemCode(productVariantPriceStockAndImagesRequest.getSkuCode())
        .pickupPointCode(itemPickupPointRequest.getPickupPointId())
        .stock(Optional.ofNullable(itemPickupPointRequest.getStock()).orElse(0))
        .initialPreOrderQuota(Optional.ofNullable(itemPickupPointRequest.getInitialPreOrderQuota()).orElse(0))
        .updatePreOrderQuota(updatePreOrderQuota && getBusinessPartnerFlagValue(profileResponse, Constants.BLIBLI_OMG))
        .minimumStock(Optional.ofNullable(itemPickupPointRequest.getMinimumStock()).orElse(0))
        .syncStock(Optional.ofNullable(itemPickupPointRequest.getSynchronizeStock()).orElse(false))
        .fbbActive(Optional.ofNullable(itemPickupPointRequest.getFbbActive()).orElse(false))
        .distributionPickupPoint(itemPickupPointRequest.isDistribution())
        .mppForWhEnabled(mppForWhEnabled).profileResponse(profileResponse)
        .preOrderDate(Optional.ofNullable(preOrderRequest).map(PreOrderRequest::getPreOrderDate).orElse(null))
        .build();
  }

  public static SyncStockUpdateOrInsertVo getSyncStockUpdateVo(boolean mppForWhEnabled, String businessPartnerCode,
      ProfileResponse profileResponse,
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ItemPickupPointRequest itemPickupPointRequest, Integer stock) {
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo = SyncStockUpdateOrInsertVo.builder()
        .updateOrInsertStockVo(
            getUpdateOrInsertStockVo(mppForWhEnabled, businessPartnerCode, profileResponse,
                productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null))
        .pickupPointSyncStockDtoList(Collections.singletonList(
            ItemSkuPickupPointSyncStockDto.builder().itemSku(itemPickupPointRequest.getItemSku())
                .syncStock(
                    Optional.ofNullable(itemPickupPointRequest.getSynchronizeStock()).orElse(false))
                .pickupPointCode(itemPickupPointRequest.getPickupPointId()).stock(stock).build()))
        .build();
    syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().setStock(Constants.ZERO);
    syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().setMinimumStock(Constants.ZERO);
    return syncStockUpdateOrInsertVo;
  }

  public static UpdateOrInsertStockVo getUpdateOrInsertStockVo(boolean mppForWhEnabled, String businessPartnerCode,
      ProfileResponse profileResponse, ItemSummaryListResponse savedProductData,
      QuickEditV2Request quickEditV2Request) {
    return UpdateOrInsertStockVo.builder().businessPartnerCode(businessPartnerCode)
        .productSku(savedProductData.getProductSku()).itemSku(quickEditV2Request.getItemSku())
        .itemCode(savedProductData.getItemCode()).pickupPointCode(quickEditV2Request.getPickupPointCode())
        .stock(Optional.ofNullable(quickEditV2Request.getDeltaStock()).orElse(0)).minimumStock(Constants.ZERO)
        .syncStock(Optional.ofNullable(quickEditV2Request.getUseWarehouseStock()).orElse(false))
        .fbbActive(Optional.ofNullable(quickEditV2Request.isFbbActivated()).orElse(false))
        .distributionPickupPoint(savedProductData.isDistribution())
        .mppForWhEnabled(mppForWhEnabled).profileResponse(profileResponse).build();
  }

  public static SyncStockUpdateOrInsertVo getSyncStockUpdateVo(boolean mppForWhEnabled, String businessPartnerCode,
      ProfileResponse profileResponse, ItemSummaryListResponse savedProductData,
      QuickEditV2Request quickEditV2Request, Integer stock) {
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo = SyncStockUpdateOrInsertVo.builder().updateOrInsertStockVo(
        getUpdateOrInsertStockVo(mppForWhEnabled, businessPartnerCode, profileResponse, savedProductData,
            quickEditV2Request)).pickupPointSyncStockDtoList(Collections.singletonList(
      ItemSkuPickupPointSyncStockDto.builder().itemSku(quickEditV2Request.getItemSku()).syncStock(
          Optional.ofNullable(quickEditV2Request.getUseWarehouseStock()).orElse(false))
        .pickupPointCode(quickEditV2Request.getPickupPointCode()).stock(stock).build())).build();
    syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().setStock(Constants.ZERO);
    syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().setMinimumStock(Constants.ZERO);
    return syncStockUpdateOrInsertVo;
  }

  public static boolean isFbbPickupPointWithoutPinPoint(List<PickupPointResponse> fbbPickupPointResponses) {
    return fbbPickupPointResponses.stream().anyMatch(
        pickupPointDTO -> Objects.isNull(pickupPointDTO.getGeolocation()) || Objects.isNull(
            pickupPointDTO.getGeolocation().getLatitude()) || Objects.isNull(
            pickupPointDTO.getGeolocation().getLongitude()) || Constants.DOUBLE_ZERO == pickupPointDTO.getGeolocation()
            .getLatitude() || Constants.DOUBLE_ZERO == pickupPointDTO.getGeolocation().getLongitude());
  }

  public static void updateProductItemAttributeValue(ProductCreationRequest productRequest,
      Map<String, Map<String, String>> attributeCodeAndValueValueType) {
    for (ProductItemCreationRequest productItemCreationRequest : Optional.ofNullable(
        productRequest.getProductItemRequests()).orElse(new ArrayList<>())) {
      updateProductItemAttributeValue(attributeCodeAndValueValueType, productItemCreationRequest);
      updateProductItemAttributesMap(attributeCodeAndValueValueType, productItemCreationRequest);
    }
  }

  private static void updateProductItemAttributesMap(Map<String, Map<String, String>> attributeCodeAndValueValueType,
      ProductItemCreationRequest productItemCreationRequest) {
    for (Map.Entry<String, String> attributeAndValueEntry : Optional.ofNullable(
        productItemCreationRequest.getAttributesMap()).orElse(new TreeMap<>()).entrySet()) {
      if (containsCombinedValueForAttributeAndValue(attributeAndValueEntry.getKey(), attributeAndValueEntry.getValue(),
          attributeCodeAndValueValueType)) {
        attributeAndValueEntry.setValue(
            attributeCodeAndValueValueType.get(attributeAndValueEntry.getKey()).get(attributeAndValueEntry.getValue()));
      }
    }
  }

  private static void updateProductItemAttributeValue(Map<String, Map<String, String>> attributeCodeAndValueValueType,
      ProductItemCreationRequest productItemCreationRequest) {
    for (ProductItemAttributeValueRequest productItemAttributeValueRequest : Optional.ofNullable(
        productItemCreationRequest.getProductItemAttributeValueRequests()).orElse(new ArrayList<>())) {
      AttributeRequest attributeRequest = productItemAttributeValueRequest.getAttribute();
      String value = productItemAttributeValueRequest.getValue();
      if (Objects.nonNull(attributeRequest) && containsCombinedValueForAttributeAndValue(
          attributeRequest.getAttributeCode(), value, attributeCodeAndValueValueType)) {
        productItemAttributeValueRequest.setValue(
            attributeCodeAndValueValueType.get(attributeRequest.getAttributeCode()).get(value));
      }
    }
  }

  private static boolean containsCombinedValueForAttributeAndValue(String attributeCode, String value,
      Map<String, Map<String, String>> attributeCodeAndValueValueType) {
    return attributeCodeAndValueValueType.containsKey(attributeCode) && attributeCodeAndValueValueType.get(
        attributeCode).containsKey(value);
  }

  public static Map<String, Map<String, String>> updateProductAllowedAttributeValue(
      ProductCreationRequest productRequest, Map<String, String> allowedAttributeValueIdAndValueTypeMap) {
    Map<String, Map<String, String>> attributeCodeAndValueValueType = new HashMap<>();
    for (ProductAttributeRequest productAttributeRequest : Optional.ofNullable(productRequest.getProductAttributes())
        .orElse(new ArrayList<>())) {
      AttributeRequest attributeRequest = productAttributeRequest.getAttribute();
      List<ProductAttributeValueRequest> productAttributeValueRequests =
          productAttributeRequest.getProductAttributeValues();
      if (isAttributeRequestNotNullAndValuesNotEmpty(attributeRequest, productAttributeValueRequests)) {
        updateProductAllowedAttributeValue(allowedAttributeValueIdAndValueTypeMap, productAttributeValueRequests,
            attributeCodeAndValueValueType, attributeRequest);
      }
    }
    return attributeCodeAndValueValueType;
  }

  private static void updateProductAllowedAttributeValue(Map<String, String> allowedAttributeValueIdAndValueTypeMap,
      List<ProductAttributeValueRequest> productAttributeValueRequests,
      Map<String, Map<String, String>> attributeCodeAndValueValueType, AttributeRequest attributeRequest) {
    for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeValueRequests) {
      AllowedAttributeValueRequest allowedAttributeValueRequest =
          productAttributeValueRequest.getAllowedAttributeValue();
      if (allowedAttributeValueTypeMapContains(allowedAttributeValueIdAndValueTypeMap, allowedAttributeValueRequest)) {
        updateAllowedAttributeValueAndUpdateMap(allowedAttributeValueIdAndValueTypeMap, attributeCodeAndValueValueType,
            attributeRequest, allowedAttributeValueRequest);
      }
    }
  }

  private static void updateAllowedAttributeValueAndUpdateMap(
      Map<String, String> allowedAttributeValueIdAndValueTypeMap,
      Map<String, Map<String, String>> attributeCodeAndValueValueType, AttributeRequest attributeRequest,
      AllowedAttributeValueRequest allowedAttributeValueRequest) {
    Map<String, String> valueAndCombinedValueCode =
        attributeCodeAndValueValueType.getOrDefault(attributeRequest.getAttributeCode(), new HashMap<>());
    valueAndCombinedValueCode.put(allowedAttributeValueRequest.getValue(),
        allowedAttributeValueIdAndValueTypeMap.get(allowedAttributeValueRequest.getId()));
    attributeCodeAndValueValueType.put(attributeRequest.getAttributeCode(), valueAndCombinedValueCode);
    allowedAttributeValueRequest.setValue(
        allowedAttributeValueIdAndValueTypeMap.get(allowedAttributeValueRequest.getId()));
  }

  private static boolean allowedAttributeValueTypeMapContains(
      Map<String, String> allowedAttributeValueIdAndValueTypeMap,
      AllowedAttributeValueRequest allowedAttributeValueRequest) {
    return Objects.nonNull(allowedAttributeValueRequest) && allowedAttributeValueIdAndValueTypeMap.containsKey(
        allowedAttributeValueRequest.getId());
  }

  private static boolean isAttributeRequestNotNullAndValuesNotEmpty(AttributeRequest attributeRequest,
      List<ProductAttributeValueRequest> productAttributeValueRequests) {
    return Objects.nonNull(attributeRequest) && CollectionUtils.isNotEmpty(productAttributeValueRequests);
  }

  public static Set<String> getSizeChartAttributeCodeWithValuesWithoutDelimiter(ProductCreationRequest productRequest,
      String sizeChartValueTypeDelimiter) {
    Set<String> attributeIds = new HashSet<>();
    for (ProductAttributeRequest productAttributeRequest : Optional.ofNullable(productRequest.getProductAttributes())
        .orElse(new ArrayList<>())) {
      AttributeRequest attributeRequest = productAttributeRequest.getAttribute();
      List<ProductAttributeValueRequest> productAttributeValueRequests =
          productAttributeRequest.getProductAttributeValues();
      if (isDefiningAndSizeChartAttributeAndValueNotEmpty(attributeRequest, productAttributeValueRequests)) {
        getSizeChartAttributeCodeWithValuesWithoutDelimiter(sizeChartValueTypeDelimiter, productAttributeValueRequests,
            attributeIds, attributeRequest);
      }
    }
    return attributeIds;
  }

  private static void getSizeChartAttributeCodeWithValuesWithoutDelimiter(String sizeChartValueTypeDelimiter,
      List<ProductAttributeValueRequest> productAttributeValueRequests, Set<String> attributeIds,
      AttributeRequest attributeRequest) {
    for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeValueRequests) {
      AllowedAttributeValueRequest allowedAttributeValueRequest =
          productAttributeValueRequest.getAllowedAttributeValue();
      if (dontContainSizeChartDelimiter(sizeChartValueTypeDelimiter, allowedAttributeValueRequest)) {
        attributeIds.add(attributeRequest.getId());
      }
    }
  }

  private static boolean dontContainSizeChartDelimiter(String sizeChartValueTypeDelimiter,
      AllowedAttributeValueRequest allowedAttributeValueRequest) {
    return Objects.nonNull(allowedAttributeValueRequest) && !allowedAttributeValueRequest.getValue()
        .contains(sizeChartValueTypeDelimiter);
  }

  private static boolean isDefiningAndSizeChartAttributeAndValueNotEmpty(AttributeRequest attributeRequest,
      List<ProductAttributeValueRequest> productAttributeValueRequests) {
    return Objects.nonNull(attributeRequest) && AttributeType.DEFINING_ATTRIBUTE.equals(
        attributeRequest.getAttributeType()) && CollectionUtils.isNotEmpty(productAttributeValueRequests);
  }

  public static void validateProductAttributeRequest(ProductRequest productRequest, String sizeChartValueTypeDelimiter,
      boolean validateDuplicateDefiningProductAttributeValue) {
    if (validateDuplicateDefiningProductAttributeValue) {
      Map<String, Set<String>> attributeCodeAndValueAndValueTypeCount = new HashMap<>();
      for (ProductAttributeRequest productAttributeRequest : Optional.ofNullable(productRequest.getProductAttributes())
          .orElse(new ArrayList<>())) {
        validateProductAttributeRequest(sizeChartValueTypeDelimiter, productAttributeRequest,
            attributeCodeAndValueAndValueTypeCount);
      }
    }
  }

  private static void validateProductAttributeRequest(String sizeChartValueTypeDelimiter,
      ProductAttributeRequest productAttributeRequest,
      Map<String, Set<String>> attributeCodeAndValueAndValueTypeCount) {
    if (isValuesNotEmpty(productAttributeRequest)) {
      for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest.getProductAttributeValues()) {
        if (isAllowedAttributeValueNotEmpty(productAttributeValueRequest)) {
          validateForDuplicateAttributeValues(sizeChartValueTypeDelimiter, productAttributeRequest,
              productAttributeValueRequest.getAllowedAttributeValue().getValue(),
              attributeCodeAndValueAndValueTypeCount);
        } else if (isPreDefinedValueNotEmpty(productAttributeValueRequest)) {
          validateForDuplicateAttributeValues(sizeChartValueTypeDelimiter, productAttributeRequest,
              productAttributeValueRequest.getPredefinedAllowedAttributeValue().getValue(),
              attributeCodeAndValueAndValueTypeCount);
        } else if (StringUtils.isNotBlank(productAttributeValueRequest.getDescriptiveAttributeValue())) {
          validateForDuplicateAttributeValues(sizeChartValueTypeDelimiter, productAttributeRequest,
              productAttributeValueRequest.getDescriptiveAttributeValue(), attributeCodeAndValueAndValueTypeCount);
        }
      }
    }
  }

  private static void validateForDuplicateAttributeValues(String sizeChartValueTypeDelimiter,
      ProductAttributeRequest productAttributeRequest, String value,
      Map<String, Set<String>> attributeCodeAndValueAndValueTypeCount) {
    String sanitizedValue = sanitizeAllowedAttributeValue(value, sizeChartValueTypeDelimiter);
    Set<String> values =
        attributeCodeAndValueAndValueTypeCount.getOrDefault(productAttributeRequest.getAttribute().getAttributeCode(),
            new HashSet<>());
    if (values.contains(sanitizedValue)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INVALID_ADD_DELETE_DUPLICATE_REQUEST.getDesc());
    } else {
      values.add(sanitizedValue);
    }
    attributeCodeAndValueAndValueTypeCount.put(productAttributeRequest.getAttribute().getAttributeCode(), values);
  }

  private static boolean isAllowedAttributeValueNotEmpty(ProductAttributeValueRequest productAttributeValueRequest) {
    return Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue()) && StringUtils.isNotEmpty(
        productAttributeValueRequest.getAllowedAttributeValue().getValue())
        && !productAttributeValueRequest.isMarkForDelete() && !productAttributeValueRequest.getAllowedAttributeValue()
        .isMarkForDelete();
  }

  private static boolean isPreDefinedValueNotEmpty(ProductAttributeValueRequest productAttributeValueRequest) {
    return Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue()) && StringUtils.isNotEmpty(
        productAttributeValueRequest.getPredefinedAllowedAttributeValue().getValue())
        && !productAttributeValueRequest.isMarkForDelete()
        && !productAttributeValueRequest.getPredefinedAllowedAttributeValue().isMarkForDelete();
  }

  private static boolean isValuesNotEmpty(ProductAttributeRequest productAttributeRequest) {
    return CollectionUtils.isNotEmpty(productAttributeRequest.getProductAttributeValues());
  }

  private static String sanitizeAllowedAttributeValue(String value, String sizeChartValueTypeDelimiter) {
    String[] tokens = value.split(sizeChartValueTypeDelimiter);
    return tokens[tokens.length - 1];
  }

  public static boolean isFaasEligibleSeller(boolean faasFeatureSwitch, ProfileResponse profileResponse) {
    return faasFeatureSwitch && getBusinessPartnerFlagValue(profileResponse, Constants.FAAS_ACTIVATED);
  }

  public static boolean getBusinessPartnerFlagValue(ProfileResponse profileResponse, String flagName) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getFlags).filter(MapUtils::isNotEmpty)
        .map(flags -> flags.get(flagName)).map(Object::toString).map(Boolean::parseBoolean).orElse(false);

  }

  public static void validateForFAASSeller(ProfileResponse profileResponse,
      FbbCreatePickupPointRequest fbbCreatePickupPointRequest,
      FbbCreatePickupPointResponse fbbCreatePickupPointResponse, boolean faasFeatureSwitch) {
    if (RequestHelper.isFaasEligibleSeller(faasFeatureSwitch, profileResponse) && Boolean.TRUE.equals(
        fbbCreatePickupPointRequest.getSynchronizeStock())) {
      log.error("Sync stock true not allowed for FAAS sellers request =  {} ", fbbCreatePickupPointRequest);
      fbbCreatePickupPointResponse.setReason(ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getDesc());
      fbbCreatePickupPointResponse.setErrorCode(ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getCode());
    }
  }

  public static ListRequestDTO<InventoryDetailInfoRequestDTO> toInventoryDetailInfoRequestDTO(
    List<PickupPointDeleteRequest> pickupPointDeleteRequests, String businessPartnerCode) {
    ListRequestDTO<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList =
      new ListRequestDTO<>();
    List<InventoryDetailInfoRequestDTO> detailInfoRequestDTOList = new ArrayList<>();
    for (PickupPointDeleteRequest pickupPointDeleteRequest : pickupPointDeleteRequests) {
      InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
        new InventoryDetailInfoRequestDTO();
      inventoryDetailInfoRequestDTO.setPickupPointCode(pickupPointDeleteRequest.getPickupPointId());
      inventoryDetailInfoRequestDTO.setWebItemSku(pickupPointDeleteRequest.getItemSku());
      inventoryDetailInfoRequestDTO.setWebMerchantCode(businessPartnerCode);
      detailInfoRequestDTOList.add(inventoryDetailInfoRequestDTO);
    }
    inventoryDetailInfoRequestDTOList.setList(detailInfoRequestDTOList);
    return inventoryDetailInfoRequestDTOList;
  }


  public static List<ProductAttributeValueRequest> toProductAttributeValueRequest(
      List<ProductAttributeValue> productAttributeValuesList) {
    List<ProductAttributeValueRequest> productAttributeValueRequests = new ArrayList<>();
    for (ProductAttributeValue productAttributeValue : productAttributeValuesList) {
      ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
      productAttributeValueRequest.setId(productAttributeValue.getId());
      productAttributeValueRequest.setDescriptiveAttributeValue(productAttributeValue.getDescriptiveAttributeValue());
      if (Objects.nonNull(productAttributeValue.getDescriptiveAttributeValueType())) {
        productAttributeValueRequest.setDescriptiveAttributeValueType(
            com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.valueOf(
                productAttributeValue.getDescriptiveAttributeValueType().name()));
      }
      if (Objects.nonNull(productAttributeValue.getAllowedAttributeValue())) {
        AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
        BeanUtils.copyProperties(productAttributeValue.getAllowedAttributeValue(), allowedAttributeValueRequest);
        productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
      }
      if (Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue())) {
        PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
            new PredefinedAllowedAttributeValueRequest();
        BeanUtils.copyProperties(productAttributeValue.getPredefinedAllowedAttributeValue(),
            predefinedAllowedAttributeValueRequest);
        productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
      }
      productAttributeValueRequests.add(productAttributeValueRequest);
    }
    return productAttributeValueRequests;
  }

  public static void setProductBusinessPartnerAttribute(String storeId, String attributeId, String attributeValue, ProductBusinessPartner productBusinessPartner) {
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute.setStoreId(storeId);
    productBusinessPartnerAttribute.setAttributeId(attributeId);
    productBusinessPartnerAttribute.setValue(Optional.ofNullable(attributeValue).orElse(Constants.HYPHEN));
    productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
    productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
  }

  public static XProdAttributeMigrationEventModel getXProdAttributeMigrationModel(
    String productCode, String attributeCode, String attributeName, String attributeValue,
    boolean skuValue, String productSku) {
    XProdAttributeMigrationEventModel xProdAttributeMigrationModel =
      new XProdAttributeMigrationEventModel();
    xProdAttributeMigrationModel.setAttributeCode(attributeCode);
    xProdAttributeMigrationModel.setAttributeName(attributeName);
    xProdAttributeMigrationModel.setAttributeValue(attributeValue);
    xProdAttributeMigrationModel.setProductCode(productCode);
    xProdAttributeMigrationModel.setSkuValue(skuValue);
    xProdAttributeMigrationModel.setProductSku(productSku);
    return xProdAttributeMigrationModel;
  }

  public static void setVideoAddEditRequestForPCB(ProductRequest productRequest,
    EditProductResponse editProductResponse) {
    if (Objects.nonNull(editProductResponse) && Objects.nonNull(productRequest)) {
      if(Optional.ofNullable(editProductResponse.getVideoUpdated())
          .orElse(Boolean.FALSE)) {
        if (Objects.nonNull(editProductResponse.getVideoAddEditRequest())) {
          com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest videoAddEditRequest =
              new com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest();
          BeanUtils.copyProperties(editProductResponse.getVideoAddEditRequest(),
              videoAddEditRequest);
          productRequest.setVideoAddEditRequest(videoAddEditRequest);
        }
      }
      productRequest.setVideoUpdated(editProductResponse.getVideoUpdated());
    }
  }

  public static void setVideoAddEditRequestForXProduct(ProductLevel3 product,
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest) {
    if (Optional.ofNullable(product).map(ProductLevel3::getVideoUpdated).orElse(Boolean.FALSE)
        && Objects.nonNull(product.getVideoAddEditRequest())) {
      com.gdn.x.product.rest.web.model.request.VideoAddEditRequest videoAddEditRequest =
          new com.gdn.x.product.rest.web.model.request.VideoAddEditRequest();
      BeanUtils.copyProperties(product.getVideoAddEditRequest(), videoAddEditRequest);
      productRequest.setVideoAddEditRequest(videoAddEditRequest);
    }
  }

  public static ProductMasterDataUpdateRequest prepareMasterDataEditRequestForPCBUpdate(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    ProductMasterDataUpdateRequest productMasterDataUpdateRequest =
      new ProductMasterDataUpdateRequest();
    productMasterDataUpdateRequest.setProductCode(productMasterDataEditRequest.getProductCode());
    productMasterDataUpdateRequest.setProductSku(productMasterDataEditRequest.getProductSku());
    productMasterDataUpdateRequest.setCategoryCode(
      productMasterDataEditRequest.getCategoryCode());
    productMasterDataUpdateRequest.setCommonImages(
      masterProductEditDTO.getProductImageEditRequestList());
    productMasterDataUpdateRequest.setBusinessPartnerCode(
      productMasterDataEditRequest.getBusinessPartnerCode());
    if (productMasterDataEditRequest.getMasterDataEditChangeTypes()
      .contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE)) {
      productMasterDataUpdateRequest.setHeight(productMasterDataEditRequest.getHeight());
      productMasterDataUpdateRequest.setWidth(productMasterDataEditRequest.getWidth());
      productMasterDataUpdateRequest.setLength(productMasterDataEditRequest.getLength());
      productMasterDataUpdateRequest.setWeight(productMasterDataEditRequest.getWeight());
      productMasterDataUpdateRequest.setShippingWeight(
        productMasterDataEditRequest.getShippingWeight());
    }
    productMasterDataUpdateRequest.setSizeChartCode(
      productMasterDataEditRequest.getSizeChartCode());
    productMasterDataUpdateRequest.setUrl(productMasterDataEditRequest.getUrl());
    productMasterDataUpdateRequest.setVideoAddEditRequest(
      setVideoAddEditRequestForMasterDataEdit(productMasterDataEditRequest));
    productMasterDataUpdateRequest.setName(productMasterDataEditRequest.getProductName());
    productMasterDataUpdateRequest.setDescription(
      Optional.ofNullable(productMasterDataEditRequest.getDescription()).orElse(StringUtils.EMPTY)
        .getBytes(StandardCharsets.UTF_8));
    productMasterDataUpdateRequest.setReviewPending(
      masterProductEditDTO.getProductCollection().isReviewPending());
    productMasterDataUpdateRequest.setVideoDelete(productMasterDataEditRequest.isVideoDelete());
    return productMasterDataUpdateRequest;
  }

  private static PCBAddEditVideoRequest setVideoAddEditRequestForMasterDataEdit(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    PCBAddEditVideoRequest videoAddEditRequest = new PCBAddEditVideoRequest();
    if (Objects.nonNull(productMasterDataEditRequest.getVideoAddEditRequest())) {
      BeanUtils.copyProperties(productMasterDataEditRequest.getVideoAddEditRequest(), videoAddEditRequest);
    }
    return videoAddEditRequest;
  }

  public static ProductBasicMasterFieldsRequest getProductMasterDataEditRequest(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    return ProductBasicMasterFieldsRequest.builder()
      .productSku(productMasterDataEditRequest.getProductSku())
      .instore(productMasterDataEditRequest.isInstore()).productType(
        getProductType(productMasterDataEditRequest.getProductType()))
      .sizeChartCode(productMasterDataEditRequest.getSizeChartCode())
      .generateProductScoreNeeded(scoreGenerationNeeded(productMasterDataEditRequest)).build();
  }

  private static boolean scoreGenerationNeeded(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    Set<L3InfoUpdateChangeType> masterDataEditChangeTypes =
      Set.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE, L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE,
        L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE, L3InfoUpdateChangeType.DIMENSIONS_UPDATE,
        L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE, L3InfoUpdateChangeType.VIDEO_UPDATE);
    Set<L3InfoUpdateChangeType> requestedTypes =
      Optional.ofNullable(productMasterDataEditRequest.getMasterDataEditChangeTypes())
        .orElse(Collections.emptySet());

    return masterDataEditChangeTypes.stream().anyMatch(requestedTypes::contains);
  }

  private static ProductType getProductType(Integer productType) {
    return Optional.ofNullable(productType).flatMap(
        productTypeInRequest -> Arrays.stream(ProductType.values())
          .filter(p -> p.getCode() == productTypeInRequest).findFirst())
      .orElseThrow(ApiIncorrectInputDataException::new);
  }

  public static ProductItemUomInfoDTO getProductItemUomInfoDTO(
      ProductItemDistributionInfoRequest productItemDistributionInfoRequest) {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setSkuCode(productItemDistributionInfoRequest.getSkuCode());
    if (Objects.nonNull(productItemDistributionInfoRequest.getDistributionItemInfoRequest())) {
      productItemUomInfoDTO.setDistributionItemInfoRequest(DistributionItemInfoRequest.builder()
          .expiry(productItemDistributionInfoRequest.getDistributionItemInfoRequest().isExpiry())
          .omniChannelSku(productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku())
          .origin(productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOrigin()).build());
    }
    if (CollectionUtils.isNotEmpty(productItemDistributionInfoRequest.getDimensionsAndUOMRequest())) {
      productItemUomInfoDTO.setDimensionAndUomDTOList(
          productItemDistributionInfoRequest.getDimensionsAndUOMRequest().stream()
              .map(RequestHelper::getDimensionAndUomDTO).collect(Collectors.toList()));
    }
    return productItemUomInfoDTO;
  }

  private static DimensionAndUomDTO getDimensionAndUomDTO(
      DimensionAndUomRequest dimensionAndUomRequest) {
    DimensionAndUomDTO dimensionAndUomDTO = new DimensionAndUomDTO();
    BeanUtils.copyProperties(dimensionAndUomRequest, dimensionAndUomDTO);
    return dimensionAndUomDTO;
  }

  public static void setItemDistributionInfo(boolean ranchIntegrationEnabled, ProductRequest productRequest,
      EditProductResponse editProductResponse) {
    if (ranchIntegrationEnabled) {
      Map<String, ProductItemDistributionInfoRequest> productItemDistributionInfoRequestMap =
          Optional.ofNullable(editProductResponse.getDistributionAndUOMRequest()).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(ProductItemDistributionInfoRequest::getSkuCode, Function.identity()));
      for (ProductItemRequest productItemRequest : Optional.ofNullable(productRequest.getProductItems())
          .orElse(new ArrayList<>())) {
        if (productItemDistributionInfoRequestMap.containsKey(productItemRequest.getSkuCode())) {
          ProductItemUomInfoDTO productItemUomInfoDTO = RequestHelper.getProductItemUomInfoDTO(
              productItemDistributionInfoRequestMap.get(productItemRequest.getSkuCode()));
          productItemRequest.setProductItemUomInfoDTO(productItemUomInfoDTO);
        }
      }
    }
  }
}
