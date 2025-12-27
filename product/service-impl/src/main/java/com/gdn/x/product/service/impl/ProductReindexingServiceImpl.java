package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.gdn.x.product.service.util.ResponseHelper.setCncActivatedForBackward;
import static com.gdn.x.product.service.util.ResponseHelper.setViewConfigResponseByRequestString;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.model.entity.ItemViewConfig;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductReindexingService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;

/**
 * Created by govind on 01/08/2018 AD.
 */
@Service
public class ProductReindexingServiceImpl implements ProductReindexingService{

  @Autowired
  private ProductSearchHelperService productSearchHelper;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Value("${search.l5.reindex.api.enabled}")
  private boolean searchL5ReindexAPIEnabled;

  @Value("${override.archival.flag.at.l4.by.l3}")
  private boolean overrideArchivalFlagAtl4ByL3;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public SimpleMasterDataDetailWithProductAndItemsResponseVo
  getMasterDataProductDetailResponseByProductCodes(
      String storeId, String username, String requestId, Set<String> productCodes)
      throws Exception {
    this.productSearchHelper.validateParameters(storeId, productCodes);
    List<Product> productResult = this.productService
        .getProductsByProductCodes(storeId, productCodes, CommonConstants.productFields);
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        CommonConstants.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productCodes);
    return getMasterDataProductDetailResponse(storeId, username, requestId, productResult, null, null);
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsResponseVo
  getMasterDataProductDetailResponseByProductSkus(
      String storeId, String username, String requestId, Set<String> productSkus)
      throws Exception {
    this.productSearchHelper.validateParameters(storeId, productSkus);
    List<Product> productResult = this.productService
        .getProductsByProductSkus(storeId, productSkus,  CommonConstants.productFields, false);
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        CommonConstants.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productSkus);
    return getMasterDataProductDetailResponse(storeId, username, requestId, productResult, null, null);
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsResponseVo
  getMasterDataProductDetailResponseByItemSku(
      String storeId, String username, String requestId, String itemSku, boolean instantPickup,
      String pickupPointCode) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        CommonConstants.STORE_ID_MUST_NOT_BE_BLANK + storeId);
    checkArgument(StringUtils.isNotBlank(itemSku),
        CommonConstants.ITEM_SKU_MUST_NOT_BE_BLANK + itemSku);
    Item item = this.itemService
        .getItem(storeId, requestId, username, itemSku, true, true, false, instantPickup, pickupPointCode, false, false);
    if (Objects.isNull(item)) {
      ItemPickupPoint itemPickupPoint = getL5Item(storeId, itemSku);
      item = itemService.getItem(storeId, requestId, username, itemPickupPoint.getItemSku(), true,
          true, false, instantPickup, pickupPointCode, false, false);
      pickupPointCode = itemPickupPoint.getPickupPointCode();
    }
    checkArgument(Objects.nonNull(item) && !item.isArchived(),
        CommonConstants.RETURN_NOT_FOUND_OF_ITEM_SKU + itemSku);
    Product productResult = this.productService.getProduct(storeId, item.getProductSku());
    checkArgument(Objects.nonNull(productResult),
        CommonConstants.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productResult);
    return getMasterDataProductDetailResponse(storeId, username, requestId,
        Arrays.asList(productResult), Arrays.asList(item), pickupPointCode);
  }

  @Override
  public Page<OfflineItemDetailVo> getOfflineItemsByItemSku(
      String storeId, String username, String requestId, String itemSku, Pageable pageable) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        CommonConstants.STORE_ID_MUST_NOT_BE_BLANK + storeId);
    checkArgument(StringUtils.isNotBlank(itemSku),
        CommonConstants.ITEM_SKU_MUST_NOT_BE_BLANK + itemSku);
    Item item = this.itemService
        .getItem(storeId, requestId, username, itemSku, false);
    checkArgument(Objects.nonNull(item), CommonConstants.RETURN_NOT_FOUND_OF_ITEM_SKU + itemSku);
    checkState(item.isCncActivated(), CommonConstants.ITEM_IS_NOT_CNC_ACTIVATED);
    Page<ItemPickupPoint> itemPickupPoints = itemPickupPointService
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, item.getItemSku(), true, false, pageable);
    List<OfflineItem> offlineItems =
        CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPoints.getContent(), false, null);
    offlineItems.forEach(offlineItem -> productHelperService.constructOfflineItem(item, offlineItem));
    return new PageImpl<>(item.getOfflineItems(),
      PageRequest.of(itemPickupPoints.getNumber(), itemPickupPoints.getSize()),
      itemPickupPoints.getTotalElements());
  }

  private ItemPickupPoint getL5Item(String storeId, String itemSku) {
    ItemPickupPoint itemPickupPoint = this.itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, itemSku);
    checkArgument(Objects.nonNull(itemPickupPoint), ProductServiceImpl.ITEM_NOT_FOUND + StringUtils.SPACE + itemSku);
    return itemPickupPoint;
  }

  private SimpleMasterDataDetailWithProductAndItemsResponseVo getMasterDataProductDetailResponse(
      String storeId, String username, String requestId, List<Product> products, List<Item> items, String pickupPointCode)
      throws Exception {
    SimpleMasterDataDetailWithProductAndItemsResponseVo result = this.productSearchHelper
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username,
            requestId, products, false, items, pickupPointCode);
    this.productSearchHelper
        .setSimpleItemCatalogs(storeId, username, requestId, result.getProductAndItems(),
            result.getMasterDataProducts());
    return result;
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(
      String storeId, String username, String requestId, String itemSku, String pickupPointCode, String fetchViewConfigByChannel) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        CommonConstants.STORE_ID_MUST_NOT_BE_BLANK + storeId);
    checkArgument(StringUtils.isNotBlank(itemSku),
        CommonConstants.ITEM_SKU_MUST_NOT_BE_BLANK + itemSku);
    checkArgument(StringUtils.isNotBlank(pickupPointCode),
        CommonConstants.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK + pickupPointCode);
    if (!searchL5ReindexAPIEnabled) {
      return new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    }
    ItemAndItemPickupPointVo
        itemAndItemPickupPointVo = this.itemService.getItemDetailsFromDB(storeId, itemSku, pickupPointCode);
    Product productResult = this.productService.getProductFromDB(storeId, itemAndItemPickupPointVo.getItem().get(0).getProductSku());
    if (Objects.isNull(productResult)) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_PRODUCT.getErrorMessage(),
          ApiErrorCodes.INVALID_PRODUCT.getErrorCode());
    }
    if (productResult.isArchived()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorMessage() + itemSku,
          ApiErrorCodes.PRODUCT_IS_ARCHIVED.getErrorCode());
    }
    if (overrideArchivalFlagAtl4ByL3) {
      itemAndItemPickupPointVo.getItem().forEach(item -> item.setArchived(productResult.isArchived()));
    }
    return getMasterDataProductDetailV2Response(storeId, username, requestId,
        Arrays.asList(productResult), itemAndItemPickupPointVo.getItem(), itemAndItemPickupPointVo.getItemPickupPoints(), fetchViewConfigByChannel);
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(
      String storeId, String username, String requestId, List<String> productSkus,
      List<String> itemSkus, boolean showDeleted, int page, int pageSize, String fetchViewConfigByChannel) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        CommonConstants.STORE_ID_MUST_NOT_BE_BLANK + storeId);
    if (Boolean.parseBoolean(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH).getValue())) {
      checkArgument(CollectionUtils.isNotEmpty(productSkus) || CollectionUtils.isNotEmpty(itemSkus),
          CommonConstants.PRODUCT_SKU_OR_ITEM_SKU_MUST_NOT_BE_BLANK);
    } else {
      return new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    }
    ItemAndItemPickupPointVo itemAndItemPickupPointVo =
        this.itemService.getItemAndPickupPointDetails(storeId, productSkus, itemSkus, showDeleted, page, pageSize);
    Set<String> productSkusSet = itemAndItemPickupPointVo.getItem().stream().map(Item::getProductSku).collect(Collectors.toSet());
    List<Product> productResult = this.productService.getProductsByProductSkus(storeId, productSkusSet, CommonConstants.productFields, showDeleted);
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        CommonConstants.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productResult);
    overrideArchivalFlagAtl4ByL3(itemAndItemPickupPointVo.getItem(), productResult);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo result =  getMasterDataProductDetailV2Response(storeId, username, requestId,
        productResult, itemAndItemPickupPointVo.getItem(), itemAndItemPickupPointVo.getItemPickupPoints(), fetchViewConfigByChannel);
    result.setTotalL5Count(itemAndItemPickupPointVo.getTotalL5Count());
    return result;
  }

  private void overrideArchivalFlagAtl4ByL3(List<Item> items, List<Product> products) {
    if (overrideArchivalFlagAtl4ByL3) {
      Map<String, Boolean> productSkuAndArchiveFlagMap =
          products.stream().collect(Collectors.toMap(Product::getProductSku, Product::isArchived, (v1, v2) -> v2));
      for (Item item : items) {
        item.setArchived(productSkuAndArchiveFlagMap.getOrDefault(item.getProductSku(), false));
      }
    }
  }

  private SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2Response(
      String storeId, String username, String requestId, List<Product> products, List<Item> items, List<ItemPickupPoint> itemPickupPoints, String fetchViewConfigByChannel)
      throws Exception {
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo result = this.productSearchHelper
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username,
            requestId, products, items, itemPickupPoints);
    Optional.ofNullable(result.getProductAndItems()).orElse(new ArrayList<>()).stream().flatMap(
        simpleProductAndItemsAndItemPickupPointV0 -> Optional.ofNullable(
            simpleProductAndItemsAndItemPickupPointV0.getItemPickupPoints()).orElse(new ArrayList<>()).stream()).forEach(
        simpleItemPickupPointVO -> {
          simpleItemPickupPointVO.setCncActive
              (setCncActivatedForBackward(simpleItemPickupPointVO.getItemViewConfigs(),
                  cncForWarehouseFeatureSwitch, simpleItemPickupPointVO.isCncActive(), ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
          simpleItemPickupPointVO.setItemViewConfigs
            (setViewConfigResponseByRequestString(simpleItemPickupPointVO.getItemViewConfigs(), fetchViewConfigByChannel, cncForWarehouseFeatureSwitch));
        }
    );
    this.productSearchHelper
        .setSimpleItemCatalogsInMasterData(storeId, username, requestId, result.getProductAndItems(),
            result.getMasterDataProducts());
    return result;
  }

}