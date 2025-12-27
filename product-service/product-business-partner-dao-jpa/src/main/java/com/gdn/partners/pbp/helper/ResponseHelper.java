package com.gdn.partners.pbp.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.util.BeanUtils;

import com.gda.mta.product.dto.CategoryDetailDto;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.response.B2BResponse;
import com.gda.mta.product.dto.response.BuyableScheduleResponse;
import com.gda.mta.product.dto.response.DiscoverableScheduleResponse;
import com.gda.mta.product.dto.response.ImageResponse;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductAndItemPickupPontL5Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCodeAndIdAndStateResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.master.model.PriceUpdateCriteria;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ResponseHelper {

  public static List<ItemPickupPointListingL3Response> toItemPickupPointListingL3Response(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList,
      Map<String, ItemImageResponse> itemImageResponseMap, Map<String, ProductCollection> productCollectionMap,
      Map<String, List<ProductItemWholesalePriceResponse>> productItemWholesalePriceMap,
      Map<String, ProductLevel3Inventory> productLevel3InventoryMap,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap, boolean onlyDefaultViewConfig,
      boolean setDefaultProductType, boolean overrideWholesalePriceActivatedSwitch,
      boolean cncForWarehouseFeatureSwitch, Map<String, String> valueAndValueTypeMap,
      String sizeChartValueTypeDelimiter, boolean populateNullForWholesalePriceActivated) {
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList = new ArrayList<>();
    for (ItemPickupPointListingResponse itemPickupPointListingResponse : itemPickupPointListingResponseList) {
      ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
      setProductAndItemAndItemPickupPointDetail(itemPickupPointListingL3Response, itemPickupPointListingResponse,
          setDefaultProductType, cncForWarehouseFeatureSwitch, valueAndValueTypeMap, sizeChartValueTypeDelimiter);
      setPriceDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse);
      setViewConfigDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse, onlyDefaultViewConfig);
      setItemUpcCodeAndImageDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse,
          itemImageResponseMap, productCollectionMap);
      setProductItemWholeSalePriceDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse,
          productItemWholesalePriceMap, overrideWholesalePriceActivatedSwitch, populateNullForWholesalePriceActivated);
      setProductDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse, productCollectionMap);
      setProductInventoryDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse,
          productLevel3InventoryMap);
      setProductCampaignDetails(itemPickupPointListingL3Response, itemPickupPointListingResponse, campaignPriceSkuResponseMap);

      setB2bFields(itemPickupPointListingResponse, itemPickupPointListingL3Response);
      itemPickupPointListingL3ResponseList.add(itemPickupPointListingL3Response);
    }
    return itemPickupPointListingL3ResponseList;
  }

  private static void setB2bFields(ItemPickupPointListingResponse itemPickupPointListingResponse,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    if (Objects.nonNull(itemPickupPointListingResponse.getB2bFields())) {
      B2BResponse b2BResponse = new B2BResponse();
      BeanUtils.copyProperties(itemPickupPointListingResponse.getB2bFields(), b2BResponse);
      itemPickupPointListingL3Response.setB2bFields(b2BResponse);
    }
  }

  public static Pair<Boolean, List<ItemPickupPointListingResponse>> toItemPickupPointListingResponse(
      List<ProductItemBusinessPartner> productItemBusinessPartnerList, ProductBusinessPartner productBusinessPartner,
      ProductCollection productCollection, ProductDetailResponse productDetailResponse,
      List<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList, List<ProductItemWholesalePrice> productItemWholesalePriceList,
      ProfileResponse profileResponse, boolean backfillWorngProductItemIds, boolean cncForWarehouseFeatureSwitch) {
    Map<String, ProductItemResponse> productItemResponseMap = productDetailResponse.getProductItemResponses().stream()
        .collect(Collectors.toMap(ProductItemResponse::getId, Function.identity()));
    Map<String, BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseMap =
        businessPartnerPickupPointResponseList.stream().collect(
            Collectors.toMap(BusinessPartnerPickupPointResponse::getCode, Function.identity(), (pickupPointCode1, pickupPointCode2) -> pickupPointCode2));
    Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap = productItemWholesalePriceList.stream()
        .collect(Collectors.toMap(
            productItemWholesalePrice -> CommonUtils.getItemSkuAndPickupPointKey(productItemWholesalePrice.getItemSku(),
                productItemWholesalePrice.getPickupPointCode()), Function.identity(), (v1, v2) -> v2));
    Map<Integer, ProductType> productTypeMap = Arrays.stream(ProductType.values())
        .collect(Collectors.toMap(productType -> productType.getCode(), Function.identity()));
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList = new ArrayList<>();
    boolean itemIdUpdated =
        updateProductItemIdInProductItemBusinessPartner(productCollection, productItemBusinessPartnerList,
            productItemResponseMap, backfillWorngProductItemIds);
    for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
      ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();

      ProductItemResponse productItemResponse =
          Optional.ofNullable(productItemResponseMap.get(productItemBusinessPartner.getProductItemId()))
              .orElse(new ProductItemResponse());

      ProductItemWholesalePrice productItemWholesalePrice = Optional.ofNullable(productItemWholesalePriceMap.get(
          CommonUtils.getItemSkuAndPickupPointKey(productItemBusinessPartner.getGdnProductItemSku(),
              productItemBusinessPartner.getPickupPointId()))).orElse(null);

      itemPickupPointListingResponse.setProductCode(productCollection.getProductCode());
      itemPickupPointListingResponse.setProductSku(productBusinessPartner.getGdnProductSku());
      itemPickupPointListingResponse.setSkuCode(productItemResponse.getSkuCode());
      itemPickupPointListingResponse.setItemSku(productItemBusinessPartner.getGdnProductItemSku());
      itemPickupPointListingResponse.setMerchantSku(productItemBusinessPartner.getMerchantSku());
      itemPickupPointListingResponse.setMerchantCode(productBusinessPartner.getBusinessPartnerId());
      itemPickupPointListingResponse.setItemName(productItemResponse.getGeneratedItemName());
      itemPickupPointListingResponse.setCategoryCode(productBusinessPartner.getCategoryCode());
      itemPickupPointListingResponse.setPickUpPointCode(productItemBusinessPartner.getPickupPointId());
      itemPickupPointListingResponse.setPickUpPointName(Optional.ofNullable(
              businessPartnerPickupPointResponseMap.get(productItemBusinessPartner.getPickupPointId()))
          .orElse(new BusinessPartnerPickupPointResponse()).getName());
      itemPickupPointListingResponse.setPickupPointDeliveryActive(Optional.ofNullable(
              businessPartnerPickupPointResponseMap.get(productItemBusinessPartner.getPickupPointId()))
          .orElse(new BusinessPartnerPickupPointResponse()).isDelivery());
      itemPickupPointListingResponse.setItemNumber(Integer.valueOf(productItemBusinessPartner.getGdnProductItemSku()
          .substring(productItemBusinessPartner.getGdnProductItemSku().lastIndexOf(Constants.HYPHEN) + 1)));
      itemPickupPointListingResponse.setEnableEdit(true);

      itemPickupPointListingResponse.setWholesalePriceConfigEnabled(Objects.nonNull(productItemWholesalePrice));
      itemPickupPointListingResponse.setWholesalePriceActivated(
          Objects.nonNull(productItemWholesalePrice) ? productItemWholesalePrice.isWholesalePriceActivated() : false);
      itemPickupPointListingResponse.setFbbActive(productItemBusinessPartner.isFbbActive());
      itemPickupPointListingResponse.setProductSyncStatus(true);
      itemPickupPointListingResponse.setProductType(productTypeMap.get(productItemBusinessPartner.getProductType()));
      itemPickupPointListingResponse.setPrices(Arrays.asList(
          new PriceResponse(null, Constants.DEFAULT, null, productItemBusinessPartner.getPrice(),
              productItemBusinessPartner.getSalePrice(), 0.0,
              productItemBusinessPartner.getPrice() - productItemBusinessPartner.getSalePrice(), null, null)));
      List<String> salesChannel = new ArrayList<>();
      if (Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::getSalesChannel).isPresent()) {
        salesChannel = Optional.ofNullable(profileResponse.getCompany().getSalesChannel()).orElse(new ArrayList<>());
      }
      itemPickupPointListingResponse.setViewConfigs(getViewConfigsForNeedCorrectionProducts(productItemBusinessPartner, salesChannel, cncForWarehouseFeatureSwitch));
      itemPickupPointListingResponse.setCncActive(setCncActivatedForBackward(itemPickupPointListingResponse.getViewConfigs(),
                    cncForWarehouseFeatureSwitch, productItemBusinessPartner.isCncActive()));
      itemPickupPointListingResponse.setB2bFields(getB2bFieldsForNeedCorrectionProducts(productItemBusinessPartner, salesChannel));
      itemPickupPointListingResponse.setDistribution(productItemBusinessPartner.isDistribution());
      itemPickupPointListingResponseList.add(itemPickupPointListingResponse);
    }
    return Pair.of(itemIdUpdated, itemPickupPointListingResponseList);
  }

  public static boolean updateProductItemIdInProductItemBusinessPartner(ProductCollection productCollection,
      List<ProductItemBusinessPartner> productItemBusinessPartnerList,
      Map<String, ProductItemResponse> productItemResponseMap, boolean backfillWorngProductItemIds) {
    if (backfillWorngProductItemIds && CollectionUtils.isNotEmpty(productItemBusinessPartnerList)
        && MapUtils.isNotEmpty(productItemResponseMap)) {
      Map<String, List<ProductItemBusinessPartner>> productItemBusinessPartnerGroup =
          productItemBusinessPartnerList.stream()
              .collect(Collectors.groupingBy(ProductItemBusinessPartner::getGdnProductItemSku));
      boolean isMigratedProduct =
          ProductCreationType.MIGRATION.name().equals(productCollection.getProductCreationType());
      boolean isSameSize = productItemBusinessPartnerGroup.size() == productItemResponseMap.size();
      boolean isProductItemIdDifferent =
          productItemBusinessPartnerList.stream().map(ProductItemBusinessPartner::getProductItemId)
              .anyMatch(productItemId -> !productItemResponseMap.containsKey(productItemId));
      List<List<ProductItemBusinessPartner>> productItemBusinessPartnerGroupList =
          new ArrayList<>(productItemBusinessPartnerGroup.values());
      List<ProductItemResponse> productItemResponseList = new ArrayList<>(productItemResponseMap.values());
      if (isMigratedProduct && isSameSize && isProductItemIdDifferent) {
        for (int index = 0; index < productItemResponseList.size(); index++) {
          for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerGroupList.get(index)) {
            productItemBusinessPartner.setProductItemId(productItemResponseList.get(index).getId());
          }
        }
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  }

  private static List<ViewConfigResponse> getViewConfigsForNeedCorrectionProducts(
      ProductItemBusinessPartner productItemBusinessPartner, List<String> salesChannel, boolean cncForWarehouseFeatureSwitch) {
      List<ViewConfigResponse> viewConfigs = new ArrayList<>();
      ViewConfigResponse viewConfigResponse = new ViewConfigResponse(null, Constants.DEFAULT, productItemBusinessPartner.isDisplay(),
          productItemBusinessPartner.isBuyable());
      viewConfigs.add(viewConfigResponse);
      if (cncForWarehouseFeatureSwitch) {
        ViewConfigResponse viewConfigResponseCNC = new ViewConfigResponse(null, Constants.CNC_CHANNEL, productItemBusinessPartner.isCncDiscoverable(),
            productItemBusinessPartner.isCncBuyable());
        viewConfigs.add(viewConfigResponseCNC);
      }
      if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
        ViewConfigResponse viewConfigResponseB2b = new ViewConfigResponse(null, Constants.B2B_CHANNEL, productItemBusinessPartner.isB2bDiscoverable(),
            productItemBusinessPartner.isB2bBuyable());
        viewConfigs.add(viewConfigResponseB2b);
      }
      return viewConfigs;
  }

  private static com.gdn.x.product.rest.web.model.response.B2BResponse getB2bFieldsForNeedCorrectionProducts(
      ProductItemBusinessPartner productItemBusinessPartner, List<String> salesChannel) {
    if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      return new com.gdn.x.product.rest.web.model.response.B2BResponse(productItemBusinessPartner.isB2bManaged(),
          productItemBusinessPartner.getB2bPrice());
    }
    return null;
  }

  private static void setProductCampaignDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap) {
    CampaignPriceSkuResponse campaignPriceSkuResponse;
    campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(
        CommonUtils.getItemSkuAndPickupPointKey(itemPickupPointListingResponse.getItemSku(),
            itemPickupPointListingResponse.getPickUpPointCode()));
    if (Objects.nonNull(campaignPriceSkuResponse)) {
      itemPickupPointListingL3Response.setCampaignCurrentPrice(campaignPriceSkuResponse.getCampaignPrice());
      itemPickupPointListingL3Response.setCampaignMaxPrice(campaignPriceSkuResponse.getMaxAllowedPrice());
      itemPickupPointListingL3Response.setCampaignMinPrice(campaignPriceSkuResponse.getMinAllowedPrice());
      itemPickupPointListingL3Response.setItemCampaignMapped(campaignPriceSkuResponse.isRegistered());
      itemPickupPointListingL3Response.setItemCampaignActivated(campaignPriceSkuResponse.isLive());
      itemPickupPointListingL3Response.setLockPriceUpdate(campaignPriceSkuResponse.isLockPriceUpdate());
      itemPickupPointListingL3Response.setPriceUpdateCriteria(Optional.ofNullable(campaignPriceSkuResponse.getPriceUpdateCriteria())
          .orElse(new HashSet<>()).stream().map(PriceUpdateCriteria::toString).collect(Collectors.toList()));
      itemPickupPointListingL3Response.setPriceNeedRevision(campaignPriceSkuResponse.isNeedRevision());
    }
  }

  private static void setProductInventoryDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, ProductLevel3Inventory> productLevel3InventoryMap) {
    ProductLevel3Inventory productLevel3Inventory = productLevel3InventoryMap.get(
        CommonUtils.getItemSkuAndPickupPointKey(itemPickupPointListingResponse.getItemSku(),
            itemPickupPointListingResponse.getPickUpPointCode()));
    if (Objects.nonNull(productLevel3Inventory)) {
      itemPickupPointListingL3Response.setWebSyncStock(productLevel3Inventory.isWebSyncStock());
      itemPickupPointListingL3Response.setAvailableStockLevel1(productLevel3Inventory.getWarehouseAvailable());
      itemPickupPointListingL3Response.setAvailableStockLevel2(productLevel3Inventory.getWebAvailable());
      itemPickupPointListingL3Response.setReservedStockLevel1(productLevel3Inventory.getWarehouseReserved());
      itemPickupPointListingL3Response.setReservedStockLevel2(productLevel3Inventory.getWebReserved());
      itemPickupPointListingL3Response.setMinimumStockLevel2(productLevel3Inventory.getWebMinAlert());
      itemPickupPointListingL3Response.setInitialPreOrderQuota(
          productLevel3Inventory.getInitialPreOrderQuota());
      itemPickupPointListingL3Response.setNonDistributionAvailable(
          productLevel3Inventory.getNonDistributionAvailable());
      itemPickupPointListingL3Response.setNonDistributionReserved(productLevel3Inventory.getNonDistributionReserved());
    }
  }

  private static void setProductDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, ProductCollection> productCollectionMap) {
    itemPickupPointListingL3Response.setRejected(WorkflowStates.DELETED.getValue().equals(
        Optional.ofNullable(productCollectionMap.get(itemPickupPointListingResponse.getProductCode()))
            .orElse(new ProductCollection()).getState()));
  }

  private static void setProductItemWholeSalePriceDetails(
      ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, List<ProductItemWholesalePriceResponse>> productItemWholesalePriceMap,
      boolean overrideWholesalePriceActivatedSwitch, boolean populateNullForWholesalePriceActivated) {
    List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponseList = productItemWholesalePriceMap.get(
            CommonUtils.getItemSkuAndPickupPointKey(itemPickupPointListingResponse.getItemSku(),
                itemPickupPointListingResponse.getPickUpPointCode()));
    if (CollectionUtils.isNotEmpty(productItemWholesalePriceResponseList)) {
      itemPickupPointListingL3Response.setProductItemWholesalePrices(productItemWholesalePriceResponseList);
    }
    if (overrideWholesalePriceActivatedSwitch && CollectionUtils.isEmpty(
        itemPickupPointListingL3Response.getProductItemWholesalePrices())) {
      itemPickupPointListingL3Response.setWholesalePriceConfigEnabled(false);
      if (!populateNullForWholesalePriceActivated) {
        itemPickupPointListingL3Response.setWholesalePriceActivated(false);
      }
    }
  }

  private static void setItemUpcCodeAndImageDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, ItemImageResponse> itemImageResponseMap, Map<String, ProductCollection> productCollectionMap) {

    List<ImageResponse> imageResponseList = new ArrayList<>();
    ItemImageResponse itemImageResponse = itemImageResponseMap.get(itemPickupPointListingResponse.getSkuCode());
    ProductCollection productCollection = Optional.ofNullable(productCollectionMap.get(itemPickupPointListingResponse.getProductCode())).orElse(new ProductCollection());

    if (Objects.nonNull(itemImageResponse)) {
      for (com.gdn.x.productcategorybase.dto.response.ImageResponse image : getFilteredImages(itemImageResponse.getImageResponses(), productCollection)) {
        imageResponseList.add(ImageResponse.builder().mainImages(image.isMainImage()).sequence(image.getSequence())
            .locationPath(image.getLocationPath()).markForDelete(image.isMarkForDelete())
            .activeLocation(image.isActive()).commonImage(image.isCommonImage()).build());
      }
      itemPickupPointListingL3Response.setUpcCode(itemImageResponse.getUpcCode());
      itemPickupPointListingL3Response.setImages(imageResponseList);
    }
  }

  private static List<com.gdn.x.productcategorybase.dto.response.ImageResponse> getFilteredImages(List<com.gdn.x.productcategorybase.dto.response.ImageResponse> images, ProductCollection productCollection) {
    return images.stream().filter(image -> !image.isMarkForDelete())
        .filter(image -> filterProcessedProductItemImages(image, productCollection)).collect(Collectors.toList());
  }

  private static boolean filterProcessedProductItemImages(com.gdn.x.productcategorybase.dto.response.ImageResponse image, ProductCollection productCollection) {
    if (image.isEdited()) {
      return image.isActive();
    }
    if (image.isRevised()) {
      if (WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())) {
        return true;
      } else {
        return !image.getOriginalImage();
      }
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return !image.getOriginalImage();
    }
  }

  private static void setViewConfigDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse, boolean onlyDefaultViewConfig) {
    List<ProductLevel3ViewConfigResponse> productLevel3ViewConfigResponseList = new ArrayList<>();
    for (ViewConfigResponse viewConfigResponse : itemPickupPointListingResponse.getViewConfigs()) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(viewConfigResponse, productLevel3ViewConfigResponse);
      if (Objects.nonNull(viewConfigResponse.getBuyableScheduleResponse())) {
        productLevel3ViewConfigResponse.setBuyableScheduleResponse(new BuyableScheduleResponse());
        BeanUtils.copyProperties(viewConfigResponse.getBuyableScheduleResponse(),
            productLevel3ViewConfigResponse.getBuyableScheduleResponse());
      }
      if (Objects.nonNull(viewConfigResponse.getDiscoverableScheduleResponse())) {
        productLevel3ViewConfigResponse.setDiscoverableScheduleResponse(new DiscoverableScheduleResponse());
        BeanUtils.copyProperties(viewConfigResponse.getDiscoverableScheduleResponse(),
            productLevel3ViewConfigResponse.getDiscoverableScheduleResponse());
      }
      productLevel3ViewConfigResponseList.add(productLevel3ViewConfigResponse);
    }
    if (onlyDefaultViewConfig) {
      productLevel3ViewConfigResponseList = productLevel3ViewConfigResponseList.stream().filter(
              productLevel3ViewConfigResponse -> Constants.DEFAULT.equals(productLevel3ViewConfigResponse.getChannelId()))
          .collect(Collectors.toList());
    }
    itemPickupPointListingL3Response.setViewConfigs(productLevel3ViewConfigResponseList);
  }

  private static void setPriceDetails(ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    List<ProductLevel3PriceResponse> productLevel3PriceResponseList = new ArrayList<>();
    for (PriceResponse priceResponse : itemPickupPointListingResponse.getPrices()) {
      ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(priceResponse, productLevel3PriceResponse);
      productLevel3PriceResponseList.add(productLevel3PriceResponse);
    }
    itemPickupPointListingL3Response.setPrices(productLevel3PriceResponseList);
  }

  private static void setProductAndItemAndItemPickupPointDetail(
      ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointListingResponse itemPickupPointListingResponse, boolean setDefaultProductType,
      boolean cncForWarehouseFeatureSwitch, Map<String, String> valueAndValueTypeMap,
      String sizeChartValueTypeDelimiter) {
    itemPickupPointListingL3Response.setProductCode(itemPickupPointListingResponse.getProductCode());
    itemPickupPointListingL3Response.setProductSku(itemPickupPointListingResponse.getProductSku());
    itemPickupPointListingL3Response.setSkuCode(itemPickupPointListingResponse.getSkuCode());
    itemPickupPointListingL3Response.setItemSku(itemPickupPointListingResponse.getItemSku());
    itemPickupPointListingL3Response.setMerchantSku(itemPickupPointListingResponse.getMerchantSku());
    itemPickupPointListingL3Response.setMerchantCode(itemPickupPointListingResponse.getMerchantCode());
    itemPickupPointListingL3Response.setItemName(itemPickupPointListingResponse.getItemName());
    itemPickupPointListingL3Response.setCategoryCode(itemPickupPointListingResponse.getCategoryCode());
    itemPickupPointListingL3Response.setPriceEditDisabledReason(
        itemPickupPointListingResponse.getPriceEditDisabledReason());
    itemPickupPointListingL3Response.setPickupPointCode(itemPickupPointListingResponse.getPickUpPointCode());
    itemPickupPointListingL3Response.setPickupPointName(itemPickupPointListingResponse.getPickUpPointName());
    itemPickupPointListingL3Response.setPickupPointCncActive(itemPickupPointListingResponse.isPickupPointCncActive());
    itemPickupPointListingL3Response.setPickupPointDeliveryActive(itemPickupPointListingResponse.isPickupPointDeliveryActive());
    itemPickupPointListingL3Response.setItemName(itemPickupPointListingResponse.getItemName());
    itemPickupPointListingL3Response.setProductScore(itemPickupPointListingResponse.getProductScore());
    itemPickupPointListingL3Response.setItemNumber(itemPickupPointListingResponse.getItemNumber());
    itemPickupPointListingL3Response.setVersion(itemPickupPointListingResponse.getVersion());
    itemPickupPointListingL3Response.setLateFulfillment(itemPickupPointListingResponse.isLateFulfillment());
    itemPickupPointListingL3Response.setArchived(itemPickupPointListingResponse.isArchived());
    itemPickupPointListingL3Response.setSuspended(itemPickupPointListingResponse.isSuspended());
    itemPickupPointListingL3Response.setOff2OnActiveFlag(itemPickupPointListingResponse.isOff2OnActiveFlag());
    itemPickupPointListingL3Response.setPromoBundling(itemPickupPointListingResponse.isPromoBundling());
    itemPickupPointListingL3Response.setMerchantPromoDiscount(itemPickupPointListingResponse.isMerchantPromoDiscount());
    itemPickupPointListingL3Response.setMerchantPromoDiscountActivated(
        itemPickupPointListingResponse.isMerchantPromoDiscountActivated());
    itemPickupPointListingL3Response.setPriceEditDisabled(itemPickupPointListingResponse.isPriceEditDisabled());
    itemPickupPointListingL3Response.setEnableEdit(itemPickupPointListingResponse.isEnableEdit());
    itemPickupPointListingL3Response.setWholesalePriceActivated(
        itemPickupPointListingResponse.isWholesalePriceConfigEnabled() ?
            itemPickupPointListingResponse.getWholesalePriceActivated() :
            null);
    itemPickupPointListingL3Response.setWholesalePromoActivated(
        itemPickupPointListingResponse.isWholesalePromoActivated());
    itemPickupPointListingL3Response.setWholesalePriceConfigEnabled(
        itemPickupPointListingResponse.isWholesalePriceConfigEnabled());
    itemPickupPointListingL3Response.setFlashSaleActive(itemPickupPointListingResponse.isFlashSaleActive());
    itemPickupPointListingL3Response.setCncActive(
        setCncActivatedForBackward(itemPickupPointListingResponse.getViewConfigs(),
            cncForWarehouseFeatureSwitch, itemPickupPointListingResponse.isCncActive()));
    itemPickupPointListingL3Response.setFbbActivated(itemPickupPointListingResponse.isFbbActive());
    itemPickupPointListingL3Response.setProductSyncStatus(itemPickupPointListingResponse.isProductSyncStatus());
    if (Objects.nonNull(itemPickupPointListingResponse.getProductType())) {
      itemPickupPointListingL3Response.setProductType(itemPickupPointListingResponse.getProductType().getCode());
    } else if (setDefaultProductType) {
      itemPickupPointListingL3Response.setProductType(1);
    }
    itemPickupPointListingL3Response.setPromoTypes(itemPickupPointListingResponse.getPromoTypes());
    itemPickupPointListingL3Response.setActivePromoBundlings(itemPickupPointListingResponse.getActivePromoBundlings());
    itemPickupPointListingL3Response.setFreeSample(itemPickupPointListingResponse.isFreeSample());
    populateAttributesMap(itemPickupPointListingResponse, valueAndValueTypeMap, sizeChartValueTypeDelimiter);
    itemPickupPointListingL3Response.setAttributesMap(itemPickupPointListingResponse.getAttributesMap());
    itemPickupPointListingL3Response.setDimensionsMissing(itemPickupPointListingResponse.getDimensionsMissing());
    itemPickupPointListingL3Response.setMissingFields(itemPickupPointListingResponse.getMissingFields());
    itemPickupPointListingL3Response.setDistribution(itemPickupPointListingResponse.isDistribution());
  }

  private static void populateAttributesMap(ItemPickupPointListingResponse itemPickupPointListingResponse,
      Map<String, String> valueAndValueTypeMap, String sizeChartValueTypeDelimiter) {
    if (MapUtils.isNotEmpty(valueAndValueTypeMap)) {
      for (Map.Entry<String, String> attributesMap : itemPickupPointListingResponse.getAttributesMap().entrySet()) {
        if (valueAndValueTypeMap.containsKey(attributesMap.getKey() + attributesMap.getValue())) {
          attributesMap.setValue(
              valueAndValueTypeMap.get(attributesMap.getKey() + attributesMap.getValue()) + sizeChartValueTypeDelimiter
                  + attributesMap.getValue());
        }
      }
    }
  }

  private static boolean setCncActivatedForBackward(List<ViewConfigResponse> viewConfigResponses,
      boolean cncWarehouseFeatureSwitch, boolean cncActivated) {
    if (cncWarehouseFeatureSwitch) {
      ViewConfigResponse viewConfigResponseCnc = viewConfigResponses.stream()
          .filter(viewConfigResponse -> viewConfigResponse.getChannelId().equals(Constants.CNC_CHANNEL))
          .findFirst().orElse(new ViewConfigResponse());
      return viewConfigResponseCnc.isBuyable();
    }
    return cncActivated;
  }

  public static InProgressProductResponse getInProgressProductResponseSettingProductSku(
      ProductCodeAndIdAndStateResponse productCodeAndIdAndStateResponse,
      Map<String, String> productBusinessPartnerIdAndProductSkuMap) {
    InProgressProductResponse inProgressProductResponse = new InProgressProductResponse();
    inProgressProductResponse.setProductSku(
        productBusinessPartnerIdAndProductSkuMap.get(productCodeAndIdAndStateResponse.getProductBusinessPartnerId()));
    inProgressProductResponse.setProductCode(productCodeAndIdAndStateResponse.getProductCode());
    inProgressProductResponse.setStatus(productCodeAndIdAndStateResponse.getState());
    return inProgressProductResponse;
  }

  public static List<InProgressProductResponse> getInProgressProductResponse(
      ProductCodeAndIdAndStateResponse productCodeAndIdAndStateResponse,
      Map<String, List<String>> productBusinessPartnerIdAndItemSkuListMap) {
    List<InProgressProductResponse> inProgressProductResponseList = new ArrayList<>();
    for (String itemSku : productBusinessPartnerIdAndItemSkuListMap.get(
        productCodeAndIdAndStateResponse.getProductBusinessPartnerId())) {
      InProgressProductResponse inProgressProductResponse = new InProgressProductResponse();
      inProgressProductResponse.setGdnProductItemSku(itemSku);
      inProgressProductResponse.setProductCode(productCodeAndIdAndStateResponse.getProductCode());
      inProgressProductResponse.setStatus(productCodeAndIdAndStateResponse.getState());
      inProgressProductResponseList.add(inProgressProductResponse);
    }
    return inProgressProductResponseList;
  }

  public static CategoryDetailDto generateCategoryNameIdAndHierarchy(List<CategoryResponse> categories)
    throws Exception {
    CategoryDetailDto categoryDetailDto = new CategoryDetailDto();
    StringBuilder categoryHierarchy = new StringBuilder();
    StringBuilder categoryHierarchyEnglish = new StringBuilder();
    int i = 0;
    ListIterator<CategoryResponse> iterator = categories.listIterator(categories.size());
    while (iterator.hasPrevious()) {
      CategoryResponse category = iterator.previous();
      if (i == categories.size() - 1) {
        categoryDetailDto.setCategoryName(category.getName());
        categoryDetailDto.setCategoryNameEnglish(category.getNameEnglish());
        categoryDetailDto.setCategoryId(category.getId());
      }
      categoryHierarchy.append(category.getName());
      if (categories.size() > 1 && i < categories.size() - 1) {
        categoryHierarchy.append(" > ");
      }
      categoryHierarchyEnglish.append(category.getNameEnglish());
      if (categories.size() > 1 && i < categories.size() - 1) {
        categoryHierarchyEnglish.append(" > ");
      }
      i++;
    }
    categoryDetailDto.setCategoryHierarchy(categoryHierarchy.toString());
    categoryDetailDto.setCategoryHierarchyEnglish(categoryHierarchyEnglish.toString());
    return categoryDetailDto;
  }

  public static boolean isProductEligibleForVendorPublish(int action, boolean isAutoNeedRevision,
      boolean trustedSeller) {
    return trustedSeller || RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType() != action && (
        RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() != action
            || isAutoNeedRevision);
  }

  public static List<ProductAndItemPickupPontL5Response> toProductDetailResponse(
    List<ProductL5DetailResponse> productL5DetailResponseList,
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap,
      Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap,
      boolean cncForWarehouseFeatureSwitch) {
    List<ProductAndItemPickupPontL5Response> pickupPontL5ResponseList = new ArrayList<>();
    for (ProductL5DetailResponse productL5DetailResponse : productL5DetailResponseList) {
      ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response =
        new ProductAndItemPickupPontL5Response();
      setProductInventoryDetailsForProductDetailL5(productAndItemPickupPontL5Response,
        productL5DetailResponse, productLevel3InventoryMap);
      setProductAndItemPickupPointDetail(productAndItemPickupPontL5Response,
        productL5DetailResponse, cncForWarehouseFeatureSwitch);
      setPriceDetails(productAndItemPickupPontL5Response, productL5DetailResponse);
      setViewConfigDetails(productAndItemPickupPontL5Response, productL5DetailResponse);
      setProductCampaignDetails(productAndItemPickupPontL5Response, productL5DetailResponse,
        campaignPriceSkuResponseMap);
      pickupPontL5ResponseList.add(productAndItemPickupPontL5Response);
    }

    return pickupPontL5ResponseList;
  }

  private static void setProductInventoryDetailsForProductDetailL5(
    ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response,
    ProductL5DetailResponse productL5DetailResponse,
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap) {
    ProductLevel3Inventory productLevel3Inventory = productLevel3InventoryMap.get(
      CommonUtils.getItemSkuAndPickupPointKey(productL5DetailResponse.getItemSku(),
        productL5DetailResponse.getPickupPointCode()));
    if (Objects.nonNull(productLevel3Inventory)) {
      productAndItemPickupPontL5Response.setWebSyncStock(productLevel3Inventory.isWebSyncStock());
      productAndItemPickupPontL5Response.setAvailableStockLevel2(
        productLevel3Inventory.getWebAvailable());
      productAndItemPickupPontL5Response.setReservedStockLevel2(
        productLevel3Inventory.getWebReserved());
    }
  }

  private static void setProductAndItemPickupPointDetail(
    ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response,
    ProductL5DetailResponse productL5DetailResponse, boolean cncForWarehouseFeatureSwitch) {
    productAndItemPickupPontL5Response.setProductCode(productL5DetailResponse.getProductCode());
    productAndItemPickupPontL5Response.setProductSku(productL5DetailResponse.getProductSku());
    productAndItemPickupPontL5Response.setSkuCode(productL5DetailResponse.getSkuCode());
    productAndItemPickupPontL5Response.setItemSku(productL5DetailResponse.getItemSku());
    productAndItemPickupPontL5Response.setMerchantSku(productL5DetailResponse.getMerchantSku());
    productAndItemPickupPontL5Response.setMerchantCode(productL5DetailResponse.getMerchantCode());
    productAndItemPickupPontL5Response.setItemName(productL5DetailResponse.getItemName());
    productAndItemPickupPontL5Response.setPriceEditDisabledReason(
      productL5DetailResponse.getPriceEditDisabledReason());
    productAndItemPickupPontL5Response.setPickupPointCode(
      productL5DetailResponse.getPickupPointCode());
    productAndItemPickupPontL5Response.setItemName(productL5DetailResponse.getItemName());
    productAndItemPickupPontL5Response.setVersion(productL5DetailResponse.getVersion());
    productAndItemPickupPontL5Response.setArchived(productL5DetailResponse.isArchived());
    productAndItemPickupPontL5Response.setPriceEditDisabled(
      productL5DetailResponse.isPriceEditDisabled());
    productAndItemPickupPontL5Response.setEnableEdit(productL5DetailResponse.isEnableEdit());
    productAndItemPickupPontL5Response.setCncActive(
        setCncActivatedForBackward(productL5DetailResponse.getViewConfigs(),
            cncForWarehouseFeatureSwitch, productL5DetailResponse.isCncActive()));
    productAndItemPickupPontL5Response.setFbbActivated(productL5DetailResponse.isFbbActivated());
    productAndItemPickupPontL5Response.setProductSyncStatus(
      productL5DetailResponse.isProductSyncStatus());
    productAndItemPickupPontL5Response.setFreeSample(productL5DetailResponse.isFreeSample());
    productAndItemPickupPontL5Response.setUpcCode(productL5DetailResponse.getUpcCode());
  }

  private static void setViewConfigDetails(
    ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response,
    ProductL5DetailResponse productL5DetailResponse) {
    List<ProductLevel3ViewConfigResponse> productLevel3ViewConfigResponseList = new ArrayList<>();
    for (ViewConfigResponse viewConfigResponse : productL5DetailResponse.getViewConfigs()) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
        new ProductLevel3ViewConfigResponse();
      BeanUtils.copyProperties(viewConfigResponse, productLevel3ViewConfigResponse);
      productLevel3ViewConfigResponseList.add(productLevel3ViewConfigResponse);
    }
    productAndItemPickupPontL5Response.setViewConfigs(productLevel3ViewConfigResponseList);
  }

  private static void setProductCampaignDetails(
    ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response,
    ProductL5DetailResponse productL5DetailResponse,
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap) {
    CampaignPriceSkuResponse campaignPriceSkuResponse;

    campaignPriceSkuResponse = campaignPriceSkuResponseMap.get(
      CommonUtils.getItemSkuAndPickupPointKey(productL5DetailResponse.getItemSku(),
        productL5DetailResponse.getPickupPointCode()));
    if (Objects.nonNull(campaignPriceSkuResponse)) {
      productAndItemPickupPontL5Response.setCampaignMaxPrice(
        campaignPriceSkuResponse.getMaxAllowedPrice());
      productAndItemPickupPontL5Response.setCampaignMinPrice(
        campaignPriceSkuResponse.getMinAllowedPrice());
      productAndItemPickupPontL5Response.setPriceUpdateCriteria(
        Optional.ofNullable(campaignPriceSkuResponse.getPriceUpdateCriteria())
          .orElse(new HashSet<>()).stream().map(PriceUpdateCriteria::toString)
          .collect(Collectors.toList()));
    }
  }

  private static void setPriceDetails(
    ProductAndItemPickupPontL5Response productAndItemPickupPontL5Response,
    ProductL5DetailResponse productL5DetailResponse) {
    List<ProductLevel3PriceResponse> productLevel3PriceResponseList = new ArrayList<>();
    for (PriceResponse priceResponse : productL5DetailResponse.getPrices()) {
      ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
      BeanUtils.copyProperties(priceResponse, productLevel3PriceResponse);
      productLevel3PriceResponseList.add(productLevel3PriceResponse);
    }
    productAndItemPickupPontL5Response.setPrices(productLevel3PriceResponseList);
  }

  public static void validateResponse(ListBaseResponse orderItemMarginsResponseListBaseResponse) throws Exception {
    if (Objects.isNull(orderItemMarginsResponseListBaseResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorCategory.DATA_NOT_FOUND.getMessage());
    }
    if (!orderItemMarginsResponseListBaseResponse.isSuccess() || Objects.isNull(
        orderItemMarginsResponseListBaseResponse.getContent())) {
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
  }

  public static EditProductV2Response toEditProductV2Response(EditProductResponse editProductResponse) {
    editProductResponse = Optional.ofNullable(editProductResponse).orElse(new EditProductResponse());
    return EditProductV2Response.builder().productReview(editProductResponse.isProductReview())
        .reviewType(editProductResponse.getReviewType()).apiErrorCode(editProductResponse.getApiErrorCode())
        .variantsErrorList(editProductResponse.getVariantsErrorList()).build();
  }

  public static void setItemPickupPointUpdateRequestForAutoCategoryChange(
    boolean takeActionOnShippingForAutoCategoryChange,
    ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    if (takeActionOnShippingForAutoCategoryChange) {
      itemPickupPointUpdateRequest.setProductType(ProductType.REGULAR);
      itemPickupPointUpdateRequest.setDimensionsMissing(true);
      itemPickupPointUpdateRequest.getAddPickupPointRequests()
        .forEach(ResponseHelper::setItemPickupPointUpdateRequestForBopisInEligibleProducts);
      itemPickupPointUpdateRequest.getQuickEditUpdateRequests()
        .forEach(ResponseHelper::setItemPickupPointUpdateRequestForBopisInEligibleProducts);
    }
  }

  private static void setItemPickupPointUpdateRequestForBopisInEligibleProducts(
    ItemPickupPointQuickEditRequest modifiedL5) {
    modifiedL5.setStatus(ProductLevel3Status.OFFLINE.name());
    modifiedL5.setScheduleUpdate(true);
    modifiedL5.setBuyableSchedule(null);
    modifiedL5.setDiscoverableSchedule(null);
  }

  public static Map<String, String> addAllowedAttributeValueWithValueTypeIsNotEmpty(AttributeResponse attributeResponse) {
    Map<String, String> allowedAttributeValueIdAndValueTypeMap = new HashMap<>();
    for (AllowedAttributeValueResponse allowedAttributeValueResponse : attributeResponse.getAllowedAttributeValues()) {
      if (StringUtils.isNotBlank(allowedAttributeValueResponse.getValueType())) {
        allowedAttributeValueIdAndValueTypeMap.put(allowedAttributeValueResponse.getId(),
            allowedAttributeValueResponse.getValue());
      }
    }
    return allowedAttributeValueIdAndValueTypeMap;
  }


  public static Map<String, Map<String, String>> getAttributeCodeAndValueAndValueTypeMap(
      MasterDataProductDTO masterDataProductDTO, String sizeChartValueTypeDelimiter) {
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap = new HashMap<>();
    for (MasterDataProductAttributeDTO masterDataProductAttributeDTO : Optional.ofNullable(masterDataProductDTO)
        .map(MasterDataProductDTO::getMasterDataProductAttributes)
        .orElse(new ArrayList<>())) {
      if (masterDataProductAttributeDTO.getMasterDataAttribute().isSizeAttribute()) {
        for (MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO : Optional.ofNullable(
            masterDataProductAttributeDTO.getMasterDataProductAttributeValues()).orElse(new ArrayList<>())) {
          Map<String, String> valueAndValueTypeMap = attributeCodeAndValueAndValueTypeMap.getOrDefault(
              masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeCode(), new HashMap<>());
          valueAndValueTypeMap.put(masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValue(),
              combineValueAndValueType(masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValueType(),
                  masterDataProductAttributeValueDTO.getAllowedAttributeValue().getValue(),
                  sizeChartValueTypeDelimiter));
          attributeCodeAndValueAndValueTypeMap.put(
              masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeCode(), valueAndValueTypeMap);
        }
      }
    }
    return attributeCodeAndValueAndValueTypeMap;
  }

  private static String combineValueAndValueType(String valueType, String value, String sizeChartValueTypeDelimiter) {
    if (StringUtils.isEmpty(valueType)) {
      return value;
    } else {
      return valueType + sizeChartValueTypeDelimiter + value;
    }
  }

  public static String getValueAndValueType(Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap,
      String attributeCode, String value, boolean valueTypeAdditionForDefiningAttributes,
      boolean concatValueAndValueTypes) {
    if (valueTypeAdditionForDefiningAttributes && concatValueAndValueTypes) {
      return Optional.ofNullable(attributeCodeAndValueAndValueTypeMap.get(attributeCode))
          .map(valueAndValueTypeMap -> valueAndValueTypeMap.get(value)).orElse(value);
    } else {
      return value;
    }
  }

  public static String getValueAndValueType(String valueType, String value, String sizeChartValueTypeDelimiter,
      boolean valueTypeAdditionForDefiningAttributes, boolean concatValueAndValueTypes) {
    if (valueTypeAdditionForDefiningAttributes && concatValueAndValueTypes) {
      return combineValueAndValueType(valueType, value, sizeChartValueTypeDelimiter);
    } else {
      return value;
    }
  }

  public static void concatValueAndValueTypesInDefiningAttributes(boolean valueTypeAdditionForDefiningAttributes,
      boolean concatValueAndValueTypes, ProductLevel3DetailsV2Response product,
      Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap) {
    for (ProductAttributeDTO productAttributeDTO : Optional.ofNullable(product.getProductL3Response())
        .map(ProductL3Response::getDefiningAttributes).orElse(new ArrayList<>())) {
      for (ProductAttributeDetailDTO productAttributeDetailDTO : Optional.ofNullable(
          productAttributeDTO.getProductAttributeDetails()).orElse(new ArrayList<>())) {
        productAttributeDetailDTO.setAttributeValue(
            ResponseHelper.getValueAndValueType(attributeCodeAndValueAndValueTypeMap,
                productAttributeDetailDTO.getAttributeCode(), productAttributeDetailDTO.getAttributeValue(),
                valueTypeAdditionForDefiningAttributes, concatValueAndValueTypes));
      }
    }
  }

  public static <T extends BaseResponse> boolean hasNextPage(GdnRestListResponse<T> response) {
    // helper method to check if GdnRestListResponse has more data to be fetched
    if (!Optional.of(response).map(GdnRestListResponse::getPageMetaData).isPresent()) {
      return false;
    }
    PageMetaData metaData = response.getPageMetaData();
    long recordsFetched = metaData.getPageNumber() * metaData.getPageSize();
    return recordsFetched < metaData.getTotalRecords();
  }

  public static void filterHideFromSellerAttributes(ProductL3DetailsResponse product, ProductL3Response productData,
      boolean productSuitabilityFeatureEnabled) {
    if (productSuitabilityFeatureEnabled && CollectionUtils.isNotEmpty(product.getAttributes())) {
      Map<String, Boolean> attributeCodeHideForSellerMap = getAttributeCodeHideForSellerMap(productData);
      product.getAttributes().removeIf(
          attribute -> Objects.nonNull(attribute) && attributeCodeHideForSellerMap.getOrDefault(
              attribute.getAttributeCode(), false));

      Optional.ofNullable(product.getProductL3Response()).map(ProductL3Response::getDescriptiveAttributes).ifPresent(
          productAttributeDetailDTOList -> productAttributeDetailDTOList.removeIf(
              attribute -> Objects.nonNull(attribute) && attributeCodeHideForSellerMap.getOrDefault(
                  attribute.getAttributeCode(), false)));

      Optional.ofNullable(product.getProductL3Response()).map(ProductL3Response::getMasterDataProduct)
          .map(MasterDataProductDTO::getMasterDataProductAttributes).ifPresent(
              masterDataProductAttributeDTOList -> masterDataProductAttributeDTOList.removeIf(
                  attribute -> Objects.nonNull(attribute) && Optional.ofNullable(attribute.getMasterDataAttribute())
                      .map(MasterDataAttributeDTO::isHideFromSeller).orElse(false)));
    }
  }

  private static Map<String, Boolean> getAttributeCodeHideForSellerMap(ProductL3Response productData) {
    return Optional.ofNullable(productData.getMasterDataProduct())
        .map(MasterDataProductDTO::getMasterDataProductAttributes).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).filter(attr -> Objects.nonNull(attr.getMasterDataAttribute())).collect(
            Collectors.toMap(attr -> attr.getMasterDataAttribute().getAttributeCode(),
                attributeDTO -> attributeDTO.getMasterDataAttribute().isHideFromSeller(),
                (existing, replacement) -> existing));
  }

  public static com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse getAiGeneratedFields(
      AiGeneratedFieldsResponse aiGeneratedFieldsResponse) {
    return Optional.ofNullable(aiGeneratedFieldsResponse).map(source -> {
      com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse
          aiGeneratedFieldsResponseFinal =
          new com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse();
      aiGeneratedFieldsResponseFinal.setAiGeneratedBrand(source.isAiGeneratedBrand());
      aiGeneratedFieldsResponseFinal.setAiGeneratedCategory(source.isAiGeneratedCategory());
      return aiGeneratedFieldsResponseFinal;
    }).orElseGet(com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse::new);
  }

}
